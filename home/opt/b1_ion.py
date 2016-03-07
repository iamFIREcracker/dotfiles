#!/usr/bin/python

import urlparse

import bunny1
from bunny1 import qp

from b1_custom import CustomCommands, CustomDecorators, CustomBunny1



class IONCommands(CustomCommands):

    def bd(self, arg):
        """Team sprint burndown"""
        return "https://docs.google.com/spreadsheets/d/1bo3GwtgoC4wdkOYiFv0nnmTwLQwZAY1wrl5IRmD7tXc/edit#gid=1335666378"

    def c(self, arg):
        """Go or search confluence.iontrading.com"""
        if arg:
            return "https://confluence.iontrading.com/dosearchsite.action?spaceSearch=false&queryString=%s" % qp(arg)
        else:
            return "https://confluence.iontrading.com/"
    def cionweb(self, arg):
        """Go to the ion.web confluence page"""
        return "https://confluence.iontrading.com/display/RD/HTML5+-+Programming+reference"

    def jr(self, arg):
        """Go or search jira.iontrading.com"""
        if arg:
            return "https://jira.iontrading.com/browse/%s" % qp(arg)
        else:
            return "https://jira.iontrading.com/"

    def jrm(self, arg):
        """Go to the JIRA markup page"""
        return "https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=all"

    def jhd(self, arg):
        """Go on the JIRA helpdesk portal"""
        return "https://jira.iontrading.com/servicedesk/customer/portals"

    def jk(self, arg):
        """Go on Jenkins, or search something on it"""
        if arg:
            return "http://axton.fssnet.internal:8080/search/?q=%s" % qp(arg)
        else:
            return "http://axton.fssnet.internal:8080/user/mlandi/my-views/view/main/"

    def jkc(self, arg):
        """Edit the configuration page of the specified Jenkins job"""
        return "http://axton.fssnet.internal:8080/view/OMS/job/%s/configure" % qp(arg)

    def jkb(self, arg):
        """Build the specified Jenkins job"""
        return "http://mlandi:7988d966bb302c66528b4d0c8f4b3f2b@axton.fssnet.internal:8080/view/OMS/job/%s/build?delay=0sec" % qp(arg)

    def jklb(self, arg):
        """Go to last build console output of the specified Jenkins job"""
        return "http://axton.fssnet.internal:8080/user/mlandi/my-views/view/main/job/%s/lastBuild/console" % arg

    def jkp(self, arg):
        """Go on the Jenkins purge build queue page"""
        return "http://axton.fssnet.internal:8080/purge-build-queue/"

    def jks(self, arg):
        """Go on the Jenkins Slicing configuration page"""
        return "http://axton.fssnet.internal:8080/slicing/"

    def jkf(self, arg):
        """Go on the last failure of the specified Jenkins job"""
        return "http://axton.fssnet.internal:8080/user/mlandi/my-views/view/main/job/%s/lastFailedBuild/console" % qp(arg)

    def np(self, arg):
        """Go or search pinexus01"""
        if arg:
            return "http://pinexus01.iontrading.com:8081/nexus/#nexus-search;quick~%s" % qp(arg)
        else:
            return "http://pinexus01.iontrading.com:8081/nexus/#welcome"

    def nu(self, arg):
        """Go or search usnexus01"""
        if arg:
            return "http://usnexus01.iontrading.com:8081/nexus/#nexus-search;quick~%s" % qp(arg)
        else:
            return "http://usnexus01.iontrading.com:8081/nexus/#welcome"

    def na(self, arg):
        """Go or search axton's nexus"""
        if arg:
            return "http://axton.fssnet.internal:8081/#nexus-search;quick~%s" % qp(arg)
        else:
            return "http://axton.fssnet.internal:8081/#welcome"

    def rflog(self, arg):
        """Open Robotframework local output file"""
        return "file:///C:/Users/mlandi/workspace/xoms-git/oms-order-test/target/robotframework-reports/log.html"

    def tr(self, arg):
        """Go on tracker"""
        return "http://www.iontrading.com"

    def lilithweb(self, arg):
        return "http://lilith.fssnet.internal:7097/ionweb/ip/html5.jsp#"

    def lilithwebadmin(self, arg):
        return "http://lilith.fssnet.internal:7097/ionweb/admin"

    def tb(self, arg):
        """Sprint tech backlog"""
        return "https://docs.google.com/spreadsheets/d/1JkVSxvv8gQJtKxeOGx6hJJ93XDM66DlwI4Pn6yBkbUc/edit#gid=197286876"

    def w(self, arg):
        """Open webserver.  Optional arg: 'lilith'"""
        if 'lilith' in arg:
            return "http://lilith.fssnet.internal:7095/ionweb/ip/html5.jsp#"
        return "http://pisa092:8080/ionweb/ip/html5.jsp#"

    def wa(self, arg):
        """Open webadmin gui.  Optional arg: 'lilith'"""
        if 'lilith' in arg:
            return "http://lilith.fssnet.internal:7095/ionweb/admin"
        return "http://pisa092:8080/ionweb/admin"

class IONDecorators(CustomDecorators):
    pass


class IONBunny(CustomBunny1):
    def __init__(self, commands, decorators):
        CustomBunny1.__init__(self, commands, decorators)


if __name__ == "__main__":
    bunny1.main(IONBunny(IONCommands(), IONDecorators()))
