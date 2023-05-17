import os
import datetime
import argparse
from dateutil.parser import parse

def rename_files(directory, deployment):
    # Loop over all files in the directory
    for filename in os.listdir(directory):
        # Check if the file extension is .spt
        if filename.endswith(".spt"):
            # Extract timestamp from the filename
            timestamp_str = filename.split('.')[0]
            timestamp = parse(timestamp_str.replace('T', ' ').replace('h', ':').replace('Z', ''))

            # Subtract 30 minutes
            new_timestamp = timestamp - datetime.timedelta(minutes=30)

            # Format the new timestamp
            new_timestamp_str = new_timestamp.strftime("%Y-%m-%d %Hh%M")

            # Rename the file
            new_filename = f'{deployment}}}{new_timestamp_str}.spt'
            os.rename(os.path.join(directory, filename), os.path.join(directory, new_filename))

    print("All .spt files have been renamed.")

def main():
    # Create argument parser
    parser = argparse.ArgumentParser(description='Renames all .spt files in a directory by subtracting 30 minutes from the timestamp in the filename and adding a prefix.')
    
    # Add arguments
    parser.add_argument('deployment', type=str, help='The WaveNet deployment name e.g. WESTGAB2WN_008')
    parser.add_argument('directory', type=str, help='The directory that contains the .spt files.')
    
    # Parse arguments
    args = parser.parse_args()
    
    # Call the rename_files function with the parsed arguments
    rename_files(args.directory, args.deployment)

if __name__ == "__main__":
    main()
