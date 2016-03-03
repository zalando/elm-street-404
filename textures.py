#!/usr/bin/env python
#
# Script to convert horizontal strips into textures (size is a power of two)
# Requires Pillow (pip install Pillow)
#
import os
import fnmatch
from itertools import product
from PIL import Image
import math


SIZES = 128, 256, 512, 1024, 2048


for root, dirnames, filenames in os.walk('./img'):
    for filename in fnmatch.filter(filenames, '*.png'):
        image_path = os.path.join(root, filename)
        dest_path = os.path.realpath(
            os.path.join(root, '../textures', filename)
        )
        img = Image.open(image_path)
        img_width, frame_height = img.size
        input_frames = raw_input('Number of frames in %s (1)? ' % filename)
        try:
            frames = int(input_frames)
        except ValueError:
            frames = 1

        frame_width = int(img_width / frames)

        possible_widths = (s for s in SIZES if s >= frame_width)
        possible_heights = (s for s in SIZES if s >= frame_height)

        def fits(size):
            width, height = size
            cols = width // frame_width
            rows = int(math.ceil(1.0 * frames / cols))
            return rows * frame_height <= height

        def empty_space(size):
            width, height = size
            return width * height - img_width * frame_height

        sizes_to_check = product(possible_widths, possible_heights)

        sizes_fit = [s for s in sizes_to_check if fits(s)]
        sizes = sorted(sizes_fit, key=empty_space)

        if len(sizes) < 0:
            print "No size combination for %s." % filename
        else:
            result_width, result_height = sizes[0]
            result_img = Image.new('RGBA', sizes[0])
            cols = result_width // frame_width
            for f in range(frames):
                x_dest = (f % cols) * frame_width
                y_dest = f // cols * frame_height
                x_src = f * frame_width
                y_src = 0
                frame_img = img.crop(
                    (x_src, y_src, x_src + frame_width, y_src + frame_height)
                )
                result_img.paste(frame_img, (x_dest, y_dest))
                result_img.save(dest_path)
