#!/usr/bin/python3

# Gets article-level self-archiving permission for publications
# by querying ShareYourPaper's permissions API (https://openaccessbutton.org/api)
# Focuses on the best permission

import configparser
import datetime
import json
import logging
import os
import time

import pandas as pd
import requests
import requests_cache

from ratelimit import limits, sleep_and_retry


# Set the rate limit to 1 call per 2 seconds
@sleep_and_retry
@limits(calls=1, period=2)
def call_api_server(url, doi):
    now = time.ctime(int(time.time()))
    response = requests.get(url + doi)
    print("Time: {0} / Used Cache: {1}".format(now, response.from_cache))

    if response.status_code != 200:
        raise Exception('API response: {}'.format(response.status_code))
    return response.json()


def call_api(url, doi):
    req = requests.Request('GET', url + doi)

    cache = requests_cache.get_cache()

    prepped = requests.Session().prepare_request(req)
    cache_key = cache.create_key(prepped)

    try:
        response = cache.get_response(cache_key)
    except (ImportError, TypeError):
        response = None

    if response:
        return response.json()

    return call_api_server(url, doi)


def get_parameters(output_formatted):

    # Skip DOIs which have no best_permission key or a null best_permission
    if not output_formatted.get("best_permission"):
        return None

    best_permission = output_formatted["best_permission"]

    # Can you self-archive the manuscript in any way?
    can_archive = best_permission.get("can_archive")

    # Where can the version named be archived?
    archiving_locations = best_permission.get("locations")

    if not archiving_locations:
        inst_repository = None
    else:
        inst_repository = 'institutional repository' in [location.lower() for location in archiving_locations]

    # What versions can be archived?
    versions = best_permission.get("versions")

    if not versions:
        submitted_version = None
        accepted_version = None
        published_version = None
    else:
        submitted_version = 'submittedVersion' in versions
        accepted_version = 'acceptedVersion' in versions
        published_version = 'publishedVersion' in versions

    # License required to be applied to the article
    licenses_required = best_permission.get("licences")

    # What institution is issuing the best permission?
    permission_issuer = best_permission["issuer"].get("type")

    # What is the embargo?
    embargo = best_permission.get("embargo_months")

    # If embargo is 0 months, it elapsed upon publication (TODO: make publication date)
    # If embargo > 0 months, compare the calculated elapsed date to query date
    if embargo == 0:
        date_embargo_elapsed = None
        is_embargo_elapsed = True
    # If embargo_months key does not exist, we can't draw a conclusion
    elif not embargo:
        date_embargo_elapsed = None
        is_embargo_elapsed = None
    else:
        # Get today's date to compare to embargo
        today = datetime.datetime.today()
        date_embargo_elapsed = best_permission.get("embargo_end")
        is_embargo_elapsed = datetime.datetime.strptime(date_embargo_elapsed, '%Y-%m-%d') < today

    # Define a final permission that depends on several conditions being met
    permission_accepted = can_archive and inst_repository and is_embargo_elapsed and accepted_version
    permission_published = can_archive and inst_repository and is_embargo_elapsed and published_version

    return can_archive, archiving_locations, inst_repository, versions, submitted_version, accepted_version, \
           published_version, licenses_required, permission_issuer, embargo, date_embargo_elapsed, \
           is_embargo_elapsed, permission_accepted, permission_published


def jprint(obj):
    # create a formatted string of the Python JSON object
    text = json.dumps(obj, sort_keys=True, indent=4)
    print(text)


def main():
    # Define input and output filenames
    filename_oa_data = "oa-unpaywall"
    filename_syp_results = "oa-syp"

    # Configure logger
    logging.basicConfig(filename='syp-query.log',
                        level=logging.INFO,
                        format='%(message)s %(asctime)s', datefmt='%d-%m-%Y %I:%M:%S %p')
    logging.info('ShareYourPaper query date:')

    # Load paths from the config file
    cfg = configparser.ConfigParser()
    cfg.read("config.ini")

    # Define data folder
    data_folder = cfg["paths"]["data_raw"]

    # Define path to file with the data
    data_file = os.path.join(data_folder, filename_oa_data + ".csv")

    # Read input dataset containing DOIs and OA status
    data = pd.read_csv(data_file)

    # Base URL
    url = "https://api.openaccessbutton.org/permissions/"

    dois = set(data['doi'].values.tolist())
    print("Number of queried publications: ", len(dois))

    requests_cache.install_cache('permissions_cache')

    unresolved_dois = []
    no_best_perm_dois = []
    no_embargo_info_dois = []
    syp_response = []
    result = []

    # make the API request
    for doi in dois:
        print(doi)
        try:
            output = call_api(url, doi)
        except Exception as e:
            print("Exception raised with DOI:", doi, e)
            unresolved_dois.append(doi)
            syp_response.append((doi, "unresolved"))
            continue

        tmp = get_parameters(output)
        if not tmp:
            print(f"SKIPPED: {doi}")
            no_best_perm_dois.append(doi)
            syp_response.append((doi, "no_best_permission"))
            continue

        if output["best_permission"].get("embargo_months") is None:
            print(f"NO EMBARGO: {doi}")
            no_embargo_info_dois.append(doi)
            syp_response.append((doi, "no_embargo_info"))
        else:
            syp_response.append((doi, "response"))
        result.append((doi, ) + tmp)

    # Create a dataframe to store the results
    df = pd.DataFrame(result, columns=[
        'doi', 'can_archive', 'archiving_locations', 'inst_repository', 'versions',
        'submitted_version', 'accepted_version', 'published_version', 'licenses_required',
        'permission_issuer', 'embargo', 'date_embargo_elapsed', 'is_embargo_elapsed',
        'permission_accepted', 'permission_published'])

    # Convert SYP response to dataframe and merge data to create result table
    df_response = pd.DataFrame(syp_response, columns=['doi', 'syp_response'])
    merged_result = df_response.merge(df, on='doi', how='left')
    merged_result.to_csv(os.path.join(data_folder, filename_syp_results + "-permissions.csv"), index=False)

    # unresolved = pd.DataFrame(unresolved_dois, columns=['doi'])
    # no_best_perm = pd.DataFrame(no_best_perm_dois, columns=['doi'])
    # unresolved.to_csv(os.path.join(data_folder, filename_syp_results + "-unresolved-permissions.csv"), index=False)
    # no_best_perm.to_csv(os.path.join(data_folder, filename_syp_results + "-no-best-permissions.csv"), index=False)

    print("Number of unresolved DOIs: ", len(unresolved_dois))
    print("Number of DOIs without a best permission: ", len(no_best_perm_dois))
    print("Number of DOIs without embargo information: ", len(no_embargo_info_dois))


if __name__ == "__main__":
    main()
