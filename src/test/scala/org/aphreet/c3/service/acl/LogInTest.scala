package org.aphreet.c3.service.acl

import org.scalatest.{ FunSuite, BeforeAndAfterAll }
import org.mortbay.jetty.Server
import org.junit.Assert
import org.openqa.selenium.By
import org.openqa.selenium.WebDriver
import org.openqa.selenium.firefox.FirefoxDriver
import java.util.concurrent.TimeUnit
import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.webapp.WebAppContext

/**
 * Created by a-legotin on 6/5/2014.
 * @author a-legotin
 * @define Test User Logging
 * @note FireFox browser needed for Selenium WebDriver
 */
class LogInTest extends FunSuite with BeforeAndAfterAll {

  var driver: WebDriver = null
  var baseUrl: String = null
  var verificationErrors: StringBuffer = new StringBuffer

  /**
   * @define preparing for test start - run server and open web browser (Firefox)
   */
  override def beforeAll() {
    val server = new Server
    val scc = new SelectChannelConnector
    scc.setPort(8080)
    server.setConnectors(Array(scc))
    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    context.setWar("src/main/webapp")
    println(">>> STARTING EMBEDDED JETTY SERVER FOR TEST, PRESS ANY KEY TO STOP")
    server.addHandler(context)
    server.start()
    driver = new FirefoxDriver
    baseUrl = "http://localhost:8080"
    driver.manage.timeouts.implicitlyWait(30, TimeUnit.SECONDS)

  }

  /**
   * @define main method. Assert results
   */
  test("Logged username should be ADMIN") {
    driver.get(baseUrl + "/")
    driver.findElement(By.linkText("Login")).click
    driver.findElement(By.id("log inputIcon")).clear
    driver.findElement(By.id("log inputIcon")).sendKeys("admin@admin.com")
    driver.findElement(By.id("pwd inputIcon")).clear
    driver.findElement(By.id("pwd inputIcon")).sendKeys("admin")
    driver.findElement(By.name("Submit")).click
    driver.findElement(By.linkText("Admin")).click();
    val loggedInUser = driver.findElement(By.className("username")).getAttribute("value");
    Assert.assertEquals(loggedInUser, "Admin")
  }

  /**
   * @define do it after test completed.
   * @todo server stopping needed?
   */
  override def afterAll() {
    driver.quit
    val verificationErrorString: String = verificationErrors.toString
    if (!("" == verificationErrorString)) {
      fail(verificationErrorString)
    }
  }
}