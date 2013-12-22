package org.aphreet.c3.apiaccess

import net.liftweb.util.Props
import net.liftweb.common.{Full, Box, Logger}
import net.liftweb.http.LiftRules
import net.liftweb.http.provider.servlet.HTTPServletContext

import com.ifunsoftware.c3.access.{C3SystemFactory, C3System}

object C3 {
  private val log = Logger("C3")

  lazy val c3System = createC3System()

  def apply(): C3System = c3System

  private def createC3System(): C3System = {
    val domain = Props.get("c3_domain_name").openOr("anonymous")

    retrieveBundleContext match {
      case Some(bundleContext) =>
        createLocalC3System(domain, bundleContext)

      case _ =>
        createRemoteC3System(domain = domain,
                             host = Props.get("c3_host")
                               .openOrThrowException("Please define a <c3_host> configuration property!"),
                             secret = Props.get("c3_domain_secret").openOr(""),
                             proxyHost = Props.get("http_proxy_host"),
                             proxyPort = Props.get("http_proxy_port").map(_.toInt))
    }
  }

  /**
   * This method retrieves an OSGi bundle context from HTTP Servlet context object
   * @return an Some[AnyRef] containing OSGi bundle context if it presents or None if not
   */
  private def retrieveBundleContext: Option[AnyRef] = {
    Option(LiftRules.context).flatMap {
      case context: HTTPServletContext => Option(context.ctx.getAttribute("osgi-bundlecontext"))
      case _ => None
    }
  }

  private def createLocalC3System(domain: String, bundleContext: AnyRef) = {
    log.info("Found OSGi bundle context, trying to obtain local C3System instance")
    new C3SystemFactory().createLocalSystem(domain, bundleContext)
  }

  private def createRemoteC3System(domain: String, host: String, secret: String,
                                   proxyHost: Box[String], proxyPort: Box[Int]) = {
    log.info(s"Using domain $domain located at $host")

    proxyHost match {
      case Full(pHost: String) =>
        val pPort = proxyPort.openOr(8080)
        log.info(s"Using proxy server to access c3: $pHost:$pPort")

        new C3SystemFactory().createSystem(host, domain, secret, 100, pHost, pPort)

      case _ =>
        new C3SystemFactory().createSystem(host, domain, secret)
    }
  }

}