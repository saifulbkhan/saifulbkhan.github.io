#!/usr/bin/env python3

import os.path as path
from argparse import ArgumentParser
from datetime import datetime

AUTHOR = 'Saiful B. Khan'
PATH_TO_POSTS = path.join('site', 'posts')


def create_post(title, tags):
    file_name = title.lower().replace(' ', '-')
    post_file_path = path.join(PATH_TO_POSTS, '{}.md'.format(file_name))
    timestamp = datetime.now().astimezone().replace(microsecond=0).isoformat()
    with open(post_file_path, 'w') as post:
        post.write('---\n')
        post.write('date: {}\n'.format(timestamp))
        post.write('author: {}\n'.format(AUTHOR))
        post.write('title: {}\n'.format(title))
        post.write('tags:\n')
        for tag in tags:
            post.write('  - {}\n'.format(tag))
        post.write('---\n')


def main():
    parser = ArgumentParser()
    parser.add_argument('-t',
                        '--title',
                        dest='title',
                        metavar='TITLE',
                        help='title for the new post')
    parser.add_argument('--tags',
                        dest='tags',
                        metavar='TAG',
                        nargs='+',
                        help='tags to be added to the new post')
    args = parser.parse_args()
    create_post(args.title, args.tags)


if __name__ == '__main__':
    main()