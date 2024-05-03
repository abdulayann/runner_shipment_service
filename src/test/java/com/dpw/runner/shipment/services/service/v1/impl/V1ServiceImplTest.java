package com.dpw.runner.shipment.services.service.v1.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;

@ContextConfiguration(classes = {V1ServiceImpl.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class V1ServiceImplTest {
    private static ObjectMapper objectMapper;
    private static V1ErrorResponse v1ErrorResponse;
    private static String v1ErrorInString;
    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private ModelMapper modelMapper;

    @MockBean(name = "restTemplateForV1")
    private RestTemplate restTemplate;

    @MockBean
    private V1AuthHelper v1AuthHelper;

    @Autowired
    private V1ServiceImpl v1ServiceImpl;

    @MockBean
    private V1ServiceUtil v1ServiceUtil;

    @BeforeAll
    static void init() throws IOException {
        objectMapper = JsonTestUtility.getMapper();
        v1ErrorResponse = V1ErrorResponse.builder().error(V1ErrorResponse.V1Error.builder().message(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE).build()).build();
        v1ErrorInString = objectMapper.writeValueAsString(v1ErrorInString);
    }
    
    /**
     * Method under test: {@link V1ServiceImpl#fetchVesselData(Object)}
     */
    @Test
    void testFetchVesselData() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));

        // Act
        V1DataResponse actualFetchVesselDataResult = v1ServiceImpl.fetchVesselData("Request");

        // Assert
        assertNull(actualFetchVesselDataResult);
    }

    @Test
    void testFetchVesselData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(null);

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () ->v1ServiceImpl.fetchVesselData("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchVesselData3() throws RestClientException {
        // Arrange
        V1ErrorResponse.V1ErrorResponseBuilder builderResult = V1ErrorResponse.builder();
        V1ErrorResponse.V1Error error = V1ErrorResponse.V1Error.builder().message("Not all who wander are lost").build();
        V1ErrorResponse buildResult = builderResult.error(error).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<V1ErrorResponse>>any())).thenReturn(buildResult);
        ResponseEntity<Object> responseEntity = mock(ResponseEntity.class);
        when(responseEntity.getBody()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(responseEntity);

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () ->v1ServiceImpl.fetchVesselData("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(responseEntity).getBody();
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateOrgCreditLimitFromBooking(CheckCreditLimitResponse)}
     */
    @Test
    void testUpdateOrgCreditLimitFromBooking() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(new ResponseEntity<>(HttpStatus.OK));

        // Act
        var responseEntity = v1ServiceImpl.updateOrgCreditLimitFromBooking(CheckCreditLimitResponse.builder().build());

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateOrgCreditLimitFromBooking2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateOrgCreditLimitFromBooking(CheckCreditLimitResponse.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testUpdateOrgCreditLimitFromBooking3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateOrgCreditLimitFromBooking(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testUpdateOrgCreditLimitFromBooking4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateOrgCreditLimitFromBooking(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }


    /**
     * Method under test: {@link V1ServiceImpl#fetchMasterData(Object)}
     */
    @Test
    void testFetchMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createMasterData(Object)}
     */
    @Test
    void testCreateMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateMasterData(Object)} (Object)}
     */
    @Test
    void testUpdateMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }
}
