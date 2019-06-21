import os
import pytest
import tempfile
from contrib import VimwikiUnfinished


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
