# script for extracting 10min files from ferrybox zip archive
# supply script with source zip and destination folder
# script will compare files and move new ones, or overwrite smaller versions
# Tom Hull 2018

from sys import argv
from __future__ import print_function
import os
import shutil
import zipfile

script, src_zip, dest_dir = argv
print("comparing files between %s and %s" % (src_zip, dest_dir))

with zipfile.ZipFile(src_zip, 'r') as z:
    for member in z.namelist():
        filename = os.path.basename(member)
        dest_file = os.path.join(dest_dir, filename)
        # look for 10min files
        if "10minfile" in filename:
            src_file = z.getinfo(member)
            if os.path.exists(dest_file):
                print("%s already exists, checking..." % dest_file)
                if src_file.file_size == os.stat(dest_file).st_size:
                    print("files are identical, skipping...")
                else:
                    print("files are different, using larger file...")
                    if src_file.file_size > os.stat(dest_file).st_size:
                        print("taking from zip")
                        source = z.open(src_file)
                        target = file(dest_file, 'wb')
                        shutil.copyfileobj(source, target)
            else:
                print("%s is new file, moving..." % dest_file)
                source = z.open(src_file)
                target = file(dest_file, 'wb')
                shutil.copyfileobj(source, target)
        if "Sensor_configuration" in filename:
            print("%s is sensor configuration" % src_file)
            src_file = z.getinfo(member)
            if os.path.exists(dest_file):
                print("%s already exists, checking..." % dest_file)
                if src_file.file_size == os.stat(dest_file).st_size:
                    print("files are identical, skipping...")
                else:
                    print("files are different, using larger file...")
                    if src_file.file_size > os.stat(dest_file).st_size:
                        print("taking from zip")
                        source = z.open(src_file)
                        target = file(dest_file, 'wb')
                        shutil.copyfileobj(source, target)
            else:
                print("%s is new file, moving..." % dest_file)
                source = z.open(src_file)
                target = file(dest_file, 'wb')
                shutil.copyfileobj(source, target)
print("File operations done between %s and %s" % (src_zip, dest_dir))
