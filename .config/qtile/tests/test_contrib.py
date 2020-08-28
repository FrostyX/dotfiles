import os
import pytest
import tempfile
import mock
from datetime import date
from contrib import VimwikiUnfinished, DaysCounter


example_wiki_content = """# 2019-05-18

## Daily checklist

* [ ] Take a vitamin C
* [X] Eat your daily carrot!

## Todo

* [ ] Finish vimwiki article
"""


class TestVimwikiUnfinished(object):

    def setup_method(self, method):
        with tempfile.NamedTemporaryFile(suffix="foo.wiki", delete=False) as fp:
            fp.write(example_wiki_content.encode("utf-8"))
        self.tmpfile = fp.name

    def teardown_method(self, method):
        os.remove(self.tmpfile)
        assert not os.path.exists(self.tmpfile)

    def test_vimwiki_unfinished(self):
        vw = VimwikiUnfinished(path=self.tmpfile)
        assert vw.poll() == "2"

    def test_no_input_param(self):
        vw = VimwikiUnfinished()
        assert vw.poll() == "?"

        # Don't specify any input parameter
        vw = VimwikiUnfinished(today=False)
        assert vw.poll() == "!"

    def test_nonexisting_file(self):
        path = self.tmpfile + ".absolutely.nonexisting.file"
        vw = VimwikiUnfinished(path=path)
        assert vw.poll() == "?"

    def test_nonexisting_executable(self):
        executable = "/nonexisting/bin/vimwiki_unfinished_is_not_installed"
        vw = VimwikiUnfinished(path=self.tmpfile, executable=executable)
        assert vw.poll() == "!"


class FakeDate(date):
    @classmethod
    def today(cls):
        return cls(year=2020, month=8, day=27)


class TestDaysCounter(object):

    @mock.patch('contrib.date', FakeDate)
    def test_days(self):
        counter = DaysCounter(starting_date=date(year=2020, month=9, day=3))
        assert counter.poll() == "7 days"

        counter = DaysCounter(
            starting_date=date(year=2020, month=9, day=3),
            format="{D} freakin days and counting"
        )
        assert counter.poll() == "7 freakin days and counting"
