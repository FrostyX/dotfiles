#-*- coding: utf-8 -*-

import os
import sqlite3
from datetime import date
from subprocess import Popen, PIPE
from libqtile.widget import GenPollText


class VimwikiUnfinished(GenPollText):

    defaults = [
        ("executable", "vimwiki_unfinished", ""),

        # See --help for vimwiki_unfinished script
        ("filetype", "wiki", "What filetype do you use?"),
        ("today", True, "Use diary file for today"),
        ("date", None, "Use diary file for given date (in YYYY-MM-DD format)"),
        ("path", None, "Path to a vimwiki file"),
        ("section", None, "Count tasks only in specified section (e.g. '== Todo ==')"),
        # @Todo add more parameters

        ("update_interval", 10, "The delay in seconds between updates"),
    ]

    def __init__(self, **config):
        GenPollText.__init__(self, **config)
        self.add_defaults(VimwikiUnfinished.defaults)

    def func(self):
        cmd = [self.executable, "--filetype", self.filetype]
        if self.path:
            cmd.extend(["--path", self.path])
        elif self.date:
            cmd.extend(["--date", self.date])
        elif self.today:
            cmd.append("--today")

        if self.section:
            cmd.extend(["--section", self.section])

        try:
            process = Popen(cmd, stdout=PIPE, stderr=PIPE)
            stdout, _ = process.communicate()
        except FileNotFoundError:
            return "!"

        # File doesn't exist
        if process.returncode == 11:
            return "?"

        # Parameters required
        if process.returncode == 2:
            return "!"
        return stdout.strip().decode("utf-8")


class Newsboat(GenPollText):

    defaults = [
        ("update_interval", 10, "The delay in seconds between updates"),
        ("dbfile", "~/.newsboat/cache.db", "Path to newsboat sqlite database file"),
    ]

    def __init__(self, **config):
        GenPollText.__init__(self, **config)
        self.add_defaults(Newsboat.defaults)

    def func(self):
        dbfile = os.path.expanduser(self.dbfile)
        try:
            connection = sqlite3.connect(dbfile)
            cursor = connection.cursor()
            cursor.execute("select count(*) from rss_item where unread=1;")
            return str(cursor.fetchone()[0])
        except sqlite3.OperationalError:
            return "!"


class DaysCounter(GenPollText):

    defaults = [
        ("update_interval", 60 * 60, "The delay in seconds between updates"),
        ("format", "{D} days", "Format of the displayed text"),
    ]

    def __init__(self, starting_date=None, **config):
        GenPollText.__init__(self, **config)
        self.add_defaults(DaysCounter.defaults)
        self.starting_date = starting_date

    def func(self):
        delta = abs(date.today() - self.starting_date)
        return self.format.format(D=delta.days)
