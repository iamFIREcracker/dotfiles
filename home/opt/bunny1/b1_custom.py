#!/usr/bin/python

__author__ = "iamFIREcracker"
__doc__ = """
An example bunny1 server with some common commands that you might want to use.
"""
__version__ = "0.1"

import urlparse

import bunny1
from bunny1 import cherrypy
from bunny1 import qp
from bunny1 import expose
from bunny1 import dont_expose


class ExampleCommands(bunny1.Bunny1Commands):

    def crunchbase(self, arg):
        if arg:
            return "https://www.crunchbase.com/organization/%s" % qp(arg)
        else:
            return "https://www.crunchbase.com"
    cb = crunchbase

    def domain(self, arg):
        """search www.facebook.com or go there"""
        if arg:
            return "https://www.dotster.com/register/domains/?dom_lookup=%s" % qp(arg)
        else:
            return "https://www.dotster.com/"

    def fb(self, arg):
        """search www.facebook.com or go there"""
        if arg:
            return "http://www.facebook.com/s.php?q=%s&init=q" % qp(arg)
        else:
            return "http://www.facebook.com/"

    def fbdev(self, arg):
        return "https://developers.facebook.com/"

    def fbapp(self, arg):
        return "https://developers.facebook.com/apps"

    def fbexp(self, arg):
        return "https://developers.facebook.com/tools/explorer"

    def yt(self, arg):
        """Searches YouTube or goes to it"""
        if arg:
            return "http://www.youtube.com/results?search_query=%s&search_type=&aq=-1&oq=" % qp(arg)
        else:
            return "http://www.youtube.com/"

    def wikipedia(self, arg):
        """Searches Wikipedia or goes there"""
        if arg:
            return "http://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go" % qp(arg)
        else:
            return "http://en.wikipedia.org"

    def stackoverflow(self, arg):
        """Searches StackOverflow or goes there"""
        if arg:
            return "http://stackoverflow.com/search?q=%s" % qp(arg)
        else:
            return "http://stackoverflow.com"
    so = stackoverflow

    def javascript(self, arg):
        """Search StackOverflow[Javascript] or goes there"""
        return self.stackoverflow('[javascript] ' + arg)
    js = javascript

    def image(self, arg):
        """Searches Google Images or goes there"""
        if arg:
            return "https://www.google.com/search?site=imghp&tbm=isch&q=%s" % qp(arg)
        else:
            return "https://www.google.com/search?site=imghp&tbm=isch"
    im = image

    def m(self, arg):
        """Goes to Google Music"""
        return "https://play.google.com/music/listen"

    def nf(self, arg):
        return "https://www.netflix.com"

    def nfsub(self, arg):
        return "https://www.netflix.com/subtitles"

    def rxjs(self, arg):
        return "https://github.com/Reactive-Extensions/RxJS/blob/master/doc/libraries/main/rx.complete.md"

    def s(self, arg):
        return self.g(arg + ' english subtitles')

    def titanium(self, arg):
        """Search StackOverflow[titanium] or goes there"""
        if arg:
            return self.stackoverflow('[titanium] ' + arg)
        else:
            return "http://docs.appcelerator.com/titanium/latest/#!/api"
    ti = titanium

    def torrent(self, arg):
        return "https://kickass.unblocked.la/usearch/%s/" % qp(arg)
    t = torrent

    def zippyshare(self, arg):
        """Goes to Zippyshare -- it does not support GET searches :-("""
        return "http://www.searchonzippy.com"
    zp = zippyshare

    def vimrc(self, arg):
        """open sjl vimrc file on bitbucket"""
        return "https://bitbucket.org/sjl/dotfiles/src/603bb1ae9da27c6e08ab115df1cb5d8f6a1442c3/vim/vimrc?at=default"


    def yc(self, arg):
        """Goes to Hacker News"""
        return "https://news.ycombinator.com"

    def playconsole(self, arg):
        return 'https://play.google.com/apps/publish'

    def googledevconsole(self, arg):
        return 'https://console.developers.google.com/project'

    def say(self, arg):
        if arg:
            return 'http://www.howjsay.com/index.php?word=%s' % qp(arg)
        else:
            return 'http://www.howjsay.com/index.php'

    # an example of showing content instead of redirecting and also
    # using content from the filesystem
    def readme(self, arg):
        """shows the contents of the README file for this software"""
        raise bunny1.PRE(bunny1.bunny1_file("README"))

    # fallback is special method that is called if a command isn't found
    # by default, bunny1 falls back to yubnub.org which has a pretty good
    # database of commands that you would want to use, but you can configure
    # it to point anywhere you'd like.  ex. you could run a personal instance
    # of bunny1 that falls back to a company-wide instance of bunny1 which
    # falls back to yubnub or some other global redirector.  yubnub similarly
    # falls back to doing a google search, which is often what a user wants.

    @dont_expose
    def fallback(self, raw, *a, **k):

        # this code makes it so that if you put a command in angle brackets
        # (so it looks like an HTML tag), then the command will get executed.
        # doing something like this is useful when there is a server on your
        # LAN with the same name as a command that you want to use without
        # any arguments.  ex. at facebook, there is an 'svn' command and
        # the svn(.facebook.com) server, so if you type 'svn' into the
        # location bar of a browser, it goes to the server first even though
        # that's not usually what you want.  this provides a workaround for
        # that problem.
        if raw.startswith("<") and raw.endswith(">"):
            return self._b1.do_command(raw[1:-1])

        # meta-fallback
        return bunny1.Bunny1Commands.fallback(self, raw, *a, **k)


def rewrite_tld(url, new_tld):
    """changes the last thing after the dot in the netloc in a URL"""
    (scheme, netloc, path, query, fragment) = urlparse.urlsplit(url)
    domain = netloc.split(".")

    # this is just an example so we naievely assume the TLD doesn't
    # include any dots (so this breaks if you try to rewrite .co.jp
    # URLs for example)...
    domain[-1] = new_tld
    new_domain = ".".join(domain)
    return urlparse.urlunsplit((scheme, new_domain, path, query, fragment))

def tld_rewriter(new_tld):
    """returns a function that rewrites the TLD of a URL to be new_tld"""
    return expose(lambda url: rewrite_tld(url, new_tld))


class ExampleDecorators(bunny1.Bunny1Decorators):
    """decorators that show switching between TLDs"""

    # we don't really need to hardcode these since they should get handled
    # by the default case below, but we'll include them just as examples.
    com = tld_rewriter("com")
    net = tld_rewriter("net")
    org = tld_rewriter("org")
    edu = tld_rewriter("edu")

    # make it so that you can do @co.uk -- the default decorator rewrites the TLD
    def __getattr__(self, attr):
        return tld_rewriter(attr)

    @expose
    def archive(self, url):
        """shows a list of older versions of the page using the wayback machine at archive.org"""
        return "http://web.archive.org/web/*/%s" % url

    @expose
    def identity(self, url):
        """a no-op decorator"""
        return url

    @expose
    def tinyurl(self, url):
        """creates a tinyurl of the URL"""
        # we need to leave url raw here since tinyurl will actually
        # break if we send it a quoted url
        return "http://tinyurl.com/create.php?url=%s" % url

class ExampleBunny(bunny1.Bunny1):
    """An example"""
    def __init__(self):
        bunny1.Bunny1.__init__(self, ExampleCommands(), ExampleDecorators())

    # an example showing how you can handle URLs that happen before
    # the querystring by adding methods to the Bunny class instead of
    # the commands class
    @cherrypy.expose
    def header_gif(self):
        """the banner GIF for the bunny1 homepage"""
        cherrypy.response.headers["Content-Type"] = "image/gif"
        return bunny1.bunny1_file("header.gif")


if __name__ == "__main__":
    bunny1.main(ExampleBunny())
