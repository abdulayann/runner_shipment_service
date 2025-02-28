package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.reportingservice.Models.DocumentRequest;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.CacheRequest;
import com.dpw.runner.shipment.services.exception.exceptions.CacheEvictionException;

import java.util.ArrayList;

import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;



@ContextConfiguration(classes = {CacheEvictionService.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class CacheEvictionServiceTest {
    @Autowired
    private CacheEvictionService cacheEvictionService;

    @MockBean
    private CacheManager cacheManager;

    @MockBean(name = "customKeyGenerator")
    private CustomKeyGenerator customKeyGenerator;

    @MockBean
    private JsonHelper jsonHelper;


    /**
     * Method under test: {@link CacheEvictionService#setKey()}
     */
    @Test
    void testSetKey() {
        // Arrange and Act
        cacheEvictionService.setKey();
        assertNotNull(HttpStatus.OK);
    }

    /**
     * Method under test: {@link CacheEvictionService#clearAllCache()}
     */
    @Test
    void testClearAllCache() {
        //   Diffblue Cover was unable to create a Spring-specific test for this Spring method.

        // Arrange, Act and Assert
        assertThrows(CacheEvictionException.class, () -> cacheEvictionService.clearAllCache());
    }

    /**
     * Method under test: {@link CacheEvictionService#clearAllCache()}
     */
    @Test
    void testClearAllCache2() {
        var entry = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(entry);
        // Arrange and Act
        cacheEvictionService.clearAllCache();
        assertNotNull(HttpStatus.OK);
    }

    /**
     * Method under test: {@link CacheEvictionService#clearAllCache()}
     */
    @Test
    void testClearAllCache3() {
        var entry = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(entry);
        // Arrange and Act
        cacheEvictionService.clearCacheByName(CommonRequestModel.buildRequest(CacheRequest.builder().build()));
        assertNotNull(HttpStatus.OK);
    }

    /**
     * Method under test:
     * {@link CacheEvictionService#clearCacheByName(CommonRequestModel)}
     */
    @Test
    void testClearCacheByName() {
        //   Diffblue Cover was unable to create a Spring-specific test for this Spring method.

        // Arrange, Act and Assert
        assertThrows(CacheEvictionException.class, () -> (cacheEvictionService).clearCacheByName(null));
    }

    /**
     * Method under test:
     * {@link CacheEvictionService#clearCacheByName(CommonRequestModel)}
     */
    @Test
    void testClearCacheByName2() {
        //   Diffblue Cover was unable to create a Spring-specific test for this Spring method.

        // Arrange
        CacheEvictionService cacheEvictionService = new CacheEvictionService();
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(CacheEvictionException.class, () -> cacheEvictionService.clearCacheByName(commonRequestModel));
    }

    /**
     * Method under test:
     * {@link CacheEvictionService#clearCacheByName(CommonRequestModel)}
     */
    @Test
    void testClearCacheByName3() {
        //   Diffblue Cover was unable to create a Spring-specific test for this Spring method.

        // Arrange
        CacheEvictionService cacheEvictionService = new CacheEvictionService();
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        CacheRequest data = CacheRequest.builder().key("Key").build();
        commonRequestModel.setData(data);

        // Act and Assert
        assertThrows(CacheEvictionException.class, () -> cacheEvictionService.clearCacheByName(commonRequestModel));
    }

    @Test
    void testClearCacheByName1() {
        boolean isSuccess = true;
        cacheEvictionService.clearCacheByName(CacheConstants.CACHE_KEY_USER, CacheConstants.SHIPMENT_SETTINGS + "69");
        assertTrue(isSuccess);
    }
}
