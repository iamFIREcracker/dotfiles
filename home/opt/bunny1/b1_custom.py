#!/usr/bin/python

import time
import urlparse

import bunny1
from bunny1 import cherrypy
from bunny1 import qp
from bunny1 import expose
from bunny1 import dont_expose


class CustomCommands(bunny1.Bunny1Commands):
    def bb(self, arg):
        """Go to Bitbucket"""
        return "https://bitbucket.org/"

    def co(self, arg):
        """Go or search confluence.iontrading.com"""
        if arg:
            return "https://confluence.iontrading.com/dosearchsite.action?spaceSearch=false&queryString=%s" % qp(arg)
        else:
            return "https://confluence.iontrading.com/"

    def coionweb(self, arg):
        """Go to the ion.web confluence page"""
        return "https://confluence.iontrading.com/display/RD/HTML5+-+Programming+reference"


    def cb(self, arg):
        """Go to Crunchbase, or search for a specific organization"""
        if arg:
            return "https://www.crunchbase.com/organization/%s" % qp(arg)
        else:
            return "https://www.crunchbase.com"

    def cn(self, arg):
        """Goes or search connection.iontrading.com"""
        if arg:
            return "https://connection.iontrading.com/#/search/people?query=%s" % qp(arg)
        else:
            return "https://connection.iontrading.com"

    def cnl(self, arg):
        """Go or search pimatlan01.iontrading.com"""
        if arg:
            return "http://pimatlanw01.iontrading.com:9001/#/search/people?query=%s" % qp(arg)
        else:
            return "http://pimatlanw01.iontrading.com:9001/"

    def cnuat(self, arg):
        """Goes or search uat-connection.iontrading.com"""
        if arg:
            return "https://uat-connection.iontrading.com/#/search/people?query=%s" % qp(arg)
        else:
            return "https://uat-connection.iontrading.com"

    def cnint(self, arg):
        """Goes or search int-connection.iontrading.com"""
        if arg:
            return "https://int-connection.iontrading.com/#/search/people?query=%s" % qp(arg)
        else:
            return "https://int-connection.iontrading.com"

    def domain(self, arg):
        """Search www.facebook.com or go there"""
        if arg:
            return "https://www.dotster.com/register/domains/?dom_lookup=%s" % qp(arg)
        else:
            return "https://www.dotster.com/"

    def dr(self, arg):
        """Goes to or searches on Google drive"""
        if arg:
            return "https://drive.google.com/drive/search?q=%s" % qp(arg)
        else:
            return "https://drive.google.com/drive"

    def epoch(self, arg):
        """Converts epoch-with-millis to date"""
        s, ms = divmod(int(arg) or time.gmtime(), 1000)
        date = '%s.%03d' % (time.strftime('%d %b %Y %H:%M:%S', time.gmtime(s)), ms)
        raise bunny1.PRE(date)

    def fb(self, arg):
        """Search www.facebook.com or go there"""
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

    def gdevconsole(self, arg):
        """Go to the Google developer console"""
        return 'https://console.developers.google.com/project'

    def gp(self, arg):
        """Go to G+"""
        return "https://plus.google.com"

    def hd(self, arg):
        """Go on the JIRA helpdesk portal"""
        return "https://jira.iontrading.com/servicedesk/customer/portals"

    def id(self, arg):
        """Search idioms"""
        if arg:
            return "http://idios.thefreedictionary.com/%s" % qp(arg)
        else:
            return "http://idios.thefreedictionary.com/"

    def im(self, arg):
        """Searches Google Images or goes there"""
        if arg:
            return "https://www.google.com/search?site=imghp&tbm=isch&q=%s" % qp(arg)
        else:
            return "https://www.google.com/search?site=imghp&tbm=isch"

    def jk(self, arg):
        """Goes to Jenkins"""
        return "https://con-jenk-mstr.lab49.com/job/ConnectION/"

    def jr(self, arg):
        """Go or search jira.iontrading.com"""
        if arg and arg == 'qst':
            return "https://jira.iontrading.com/secure/RapidBoard.jspa?rapidView=620"
        if arg:
            return "https://jira.iontrading.com/browse/%s" % qp(arg)
        else:
            return "https://jira.iontrading.com"

    def jrtest(self, arg):
        """Go or search is-jirasupport-test.iontrading.com"""
        if arg:
            return "https://is-jirasupport-test.iontrading.com/browse/%s" % qp(arg)
        else:
            return "https://is-jirasupport-test.iontrading.com"

    def jrtpttest(self, arg):
        """Go or search https://portal.tpt.com/devjira2"""
        if arg:
            return "https://portal.tpt.com/devjira2/browse/%s" % qp(arg)
        else:
            return "https://portal.tpt.com/devjira2"
    def js(self, arg):
        """Search StackOverflow[Javascript] or goes there"""
        return self.so('[javascript] ' + arg)

    def l(self, arg):
        """Show lodash documentation"""
        return "https://lodash.com/docs"

    def m(self, arg):
        """Goes to Google Music"""
        return "https://play.google.com/music/listen"

    def nf(self, arg):
        """Go to netflix"""
        return "https://www.netflix.com"

    def nfsub(self, arg):
        """Show Netflix titles by subtitle"""
        return "https://www.netflix.com/subtitles"

    def nx49(self, arg):
        """Goes or search lab49's nexus"""
        return "https://software-repository.lab49.com/nexus/"

    def nxp(self, arg):
        """Go or search pinexus01"""
        if arg:
            return "http://pinexus01.iontrading.com:8081/nexus/#nexus-search;quick~%s" % qp(arg)
        else:
            return "http://pinexus01.iontrading.com:8081/nexus/#welcome"

    def nxu(self, arg):
        """Go or search usnexus01"""
        if arg:
            return "http://usnexus01.iontrading.com:8081/nexus/#nexus-search;quick~%s" % qp(arg)
        else:
            return "http://usnexus01.iontrading.com:8081/nexus/#welcome"

    def nxa(self, arg):
        """Go or search axton's nexus"""
        if arg:
            return "http://axton.fssnet.internal:8081/#nexus-search;quick~%s" % qp(arg)
        else:
            return "http://axton.fssnet.internal:8081/#welcome"

    def rally(self, arg):
        """Goes to Connection Delivery train"""
        return "https://rally1.rallydev.com/#/50168985559d/custom/50509556258"

    def rfdoc(self, arg):
        """Goes or searches inside robotframework.org"""
        if arg:
            return self.g('site:robotframework.org ' + arg)
        else:
            return 'http://robotframework.org'

    def rxjs(self, arg):
        """Show rxjs documentation"""
        return "https://github.com/Reactive-Extensions/RxJS/blob/master/doc/libraries/main/rx.complete.md"

    def so(self, arg):
        """Searches StackOverflow or goes there"""
        if arg:
            return self.g('site:stackoverflow.com ' + arg)
        else:
            return "http://stackoverflow.com"

    def settleup(self, arg):
        """Goes to settleup.info"""
        return "http://www.settleup.info/?web"

    def slackbot(self, arg):
        """Goes to slackbot configuration page;
        possible values 'strappo', 'xoms', 'tinyapp'"""
        return "https://%s.slack.com/customize/slackbot" % qp(arg)

    def sub(self, arg):
        """Search english subtitles on Google"""
        return self.g(arg + ' english subtitles')

    def t(self, arg):
        """Search torrents"""
        # return "https://kickass.unblocked.la/usearch/%s/" % qp(arg)
        return "http://extratorrent.cc/search/?search=%s" % qp(arg)

    def ti(self, arg):
        """Search StackOverflow[titanium] or goes there"""
        if arg:
            return self.so('[titanium] ' + arg)
        else:
            return "http://docs.appcelerator.com/titanium/latest/#!/api"

    def tv(self, arg):
        """Goes to or search tvshowtime"""
        if arg:
            return "https://www.tvshowtime.com/search?q=%s" % qp(arg)
        else:
            return "https://www.tvshowtime.com"

    def ym(self, arg):
        """Goes or search yammer"""
        if arg:
            return "https://www.yammer.com/iontrading.com/#/Threads/Search?search=%s" % qp(arg)
        else:
            return "https://www.yammer.com/iontrading.com/#/home"

    def zippyshare(self, arg):
        """Goes to Zippyshare -- it does not support GET searches :-("""
        return "http://www.searchonzippy.com"
    zp = zippyshare

    def yc(self, arg):
        """Goes to Hacker News"""
        return "https://news.ycombinator.com"

    def playconsole(self, arg):
        return 'https://play.google.com/apps/publish'

    def say(self, arg):
        """Pronounce an English word"""
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


class CustomDecorators(bunny1.Bunny1Decorators):
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


class CustomBunny1(bunny1.Bunny1):
    def __init__(self, commands, decorators):
        bunny1.Bunny1.__init__(self, commands, decorators)

    # an example showing how you can handle URLs that happen before
    # the querystring by adding methods to the Bunny class instead of
    # the commands class
    @cherrypy.expose
    def header_gif(self):
        """the banner GIF for the bunny1 homepage"""
        cherrypy.response.headers["Content-Type"] = "image/gif"
        return bunny1.bunny1_file("header.gif")


if __name__ == "__main__":
    bunny1.main(CustomBunny1(CustomCommands(), CustomDecorators()))
