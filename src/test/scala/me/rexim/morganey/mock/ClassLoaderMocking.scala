package me.rexim.morganey.mock

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import java.io._
import java.util.Vector
import java.net.URL
import java.nio.charset.StandardCharsets

trait ClassLoaderMocking extends MockitoSugar {
  def mockClassLoaderResource(resourcePath: String, resourceContent: String): ClassLoader = {
    val resourceUrl: URL = {
      val mockResourceUrl = mock[URL]
      when(mockResourceUrl.openStream())
        .thenReturn(new ByteArrayInputStream(resourceContent.getBytes(StandardCharsets.UTF_8)))
      mockResourceUrl
    }

    val classLoader: ClassLoader = {
      val mockClassLoader = mock[ClassLoader]
      when(mockClassLoader.getResource(resourcePath)).thenReturn(resourceUrl)
      mockClassLoader
    }

    classLoader
  }
}
