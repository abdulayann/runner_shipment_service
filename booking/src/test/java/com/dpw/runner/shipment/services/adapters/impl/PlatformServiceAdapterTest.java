package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformCreateRequest;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformUpdateRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {PlatformServiceAdapter.class, String.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class PlatformServiceAdapterTest {
    @MockBean
    private JsonHelper jsonHelper;

    @Autowired
    private PlatformServiceAdapter platformServiceAdapter;

    @MockBean(name = "restTemplateForPlatform")
    private RestTemplate restTemplate;

    @Test
    void testCreateAtPlatform() throws RunnerException {
        PlatformCreateRequest platformCreateRequest = PlatformCreateRequest.builder().build();
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok("abc"));
        when(jsonHelper.convertToJson(Mockito.any())).thenReturn("hello");
        ResponseEntity<IRunnerResponse> response = platformServiceAdapter.createAtPlatform(CommonRequestModel.buildRequest(platformCreateRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testUpdateAtPlatform() throws RunnerException {
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder().build();
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok("abc"));
        when(jsonHelper.convertToJson(Mockito.any())).thenReturn("hello");
        ResponseEntity<IRunnerResponse> response = platformServiceAdapter.updateAtPlaform(CommonRequestModel.buildRequest(platformUpdateRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }
}
