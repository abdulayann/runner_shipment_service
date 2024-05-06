package com.dpw.runner.shipment.services.service.v1.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
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
import java.util.List;

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

//    TODO fetchCarrierMasterData

    /**
     * Method under test: {@link V1ServiceImpl#createCarrierMasterData(Object)} (Object)}
     */
    @Test
    void testCreateCarrierMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createCarrierMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateCarrierMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCarrierMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateCarrierMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCarrierMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateCarrierMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCarrierMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateCarrierMasterData(Object)} (Object)}
     */
    @Test
    void testUpdateCarrierMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateCarrierMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateCarrierMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCarrierMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateCarrierMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCarrierMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateCarrierMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCarrierMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchContainerTypeData(Object)} (Object)}
     */
    @Test
    void testFetchContainerTypeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchContainerTypeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchContainerTypeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchContainerTypeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchContainerTypeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchContainerTypeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchContainerTypeData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchContainerTypeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createContainerTypeData(Object)} (Object)}
     */
    @Test
    void testCreateContainerTypeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createContainerTypeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateContainerTypeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createContainerTypeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateContainerTypeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createContainerTypeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateContainerTypeData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createContainerTypeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateContainerTypeData(Object)} (Object)}
     */
    @Test
    void testUpdateContainerTypeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateContainerTypeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateContainerTypeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateContainerTypeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateContainerTypeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateContainerTypeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateContainerTypeData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateContainerTypeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }



    /**
     * Method under test: {@link V1ServiceImpl#createVesselData(Object)} (Object)}
     */
    @Test
    void testCreateVesselData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createVesselData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateVesselData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createVesselData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateVesselData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createVesselData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateVesselData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createVesselData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateVesselData(Object)} (Object)}
     */
    @Test
    void testUpdateVesselData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateVesselData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateVesselData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateVesselData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateVesselData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateVesselData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateVesselData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateVesselData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchRoutingMasterData(Object)} (Object)}
     */
    @Test
    void testFetchRoutingMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchRoutingMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchRoutingMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchRoutingMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchRoutingMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchRoutingMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchRoutingMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchRoutingMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createRoutingMasterData(Object)} (Object)}
     */
    @Test
    void testCreateRoutingMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createRoutingMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateRoutingMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createRoutingMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateRoutingMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createRoutingMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateRoutingMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createRoutingMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateRoutingMasterData(Object)} (Object)}
     */
    @Test
    void testUpdateRoutingMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateRoutingMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateRoutingMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateRoutingMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateRoutingMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateRoutingMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateRoutingMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateRoutingMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createCurrenciesData(Object)} (Object)}
     */
    @Test
    void testCreateCurrenciesData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createCurrenciesData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateCurrenciesData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCurrenciesData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateCurrenciesData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCurrenciesData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateCurrenciesData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCurrenciesData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }
    /**
     * Method under test: {@link V1ServiceImpl#updateCurrenciesData(Object)} (Object)}
     */
    @Test
    void testUpdateCurrenciesData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateCurrenciesData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateCurrenciesData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCurrenciesData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateCurrenciesData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCurrenciesData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateCurrenciesData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCurrenciesData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchCurrenciesData(Object)} (Object)}
     */
    @Test
    void testFetchCurrenciesData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchCurrenciesData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchCurrenciesData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCurrenciesData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchCurrenciesData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCurrenciesData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchCurrenciesData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCurrenciesData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createDangerousGoodData(Object)} (Object)}
     */
    @Test
    void testCreateDangerousGoodData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createDangerousGoodData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateDangerousGoodData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createDangerousGoodData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateDangerousGoodData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createDangerousGoodData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateDangerousGoodData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createDangerousGoodData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }
    /**
     * Method under test: {@link V1ServiceImpl#updateDangerousGoodData(Object)} (Object)}
     */
    @Test
    void testUpdateDangerousGoodData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateDangerousGoodData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateDangerousGoodData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateDangerousGoodData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateDangerousGoodData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateDangerousGoodData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateDangerousGoodData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateDangerousGoodData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }
    /**
     * Method under test: {@link V1ServiceImpl#fetchDangerousGoodData(Object)} (Object)}
     */
    @Test
    void testFetchDangerousGoodData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchDangerousGoodData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchDangerousGoodData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchDangerousGoodData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchDangerousGoodData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchDangerousGoodData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchDangerousGoodData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchDangerousGoodData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchWarehouseData(Object)} (Object)}
     */
    @Test
    void testFetchWarehouseData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchWarehouseData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchWarehouseData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchWarehouseData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchWarehouseData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchWarehouseData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchWarehouseData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchWarehouseData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }
    /**
     * Method under test: {@link V1ServiceImpl#createWarehouseData(Object)} (Object)}
     */
    @Test
    void testCreateWarehouseData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createWarehouseData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateWarehouseData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createWarehouseData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateWarehouseData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createWarehouseData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateWarehouseData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createWarehouseData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }
    /**
     * Method under test: {@link V1ServiceImpl#updateWarehouseData(Object)} (Object)}
     */
    @Test
    void testUpdateWarehouseData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateWarehouseData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateWarehouseData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateWarehouseData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateWarehouseData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateWarehouseData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateWarehouseData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateWarehouseData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchPortsData(Object)} (Object)}
     */
    @Test
    void testFetchPortsData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchPortsData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchPortsData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchPortsData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchPortsData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchPortsData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchPortsData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchPortsData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createPortsData(Object)} (Object)}
     */
    @Test
    void testCreatePortsData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createPortsData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreatePortsData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createPortsData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreatePortsData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createPortsData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreatePortsData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createPortsData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updatePortsData(Object)} (Object)}
     */
    @Test
    void testUpdatePortsData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updatePortsData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdatePortsData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updatePortsData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdatePortsData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updatePortsData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdatePortsData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updatePortsData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchCommodityData(Object)} (Object)}
     */
    @Test
    void testFetchCommodityData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchCommodityData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchCommodityData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCommodityData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchCommodityData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCommodityData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchCommodityData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCommodityData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createCommodityData(Object)} (Object)}
     */
    @Test
    void testCreateCommodityData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createCommodityData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateCommodityData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCommodityData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateCommodityData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCommodityData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateCommodityData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createCommodityData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateCommodityData(Object)} (Object)}
     */
    @Test
    void testUpdateCommodityData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateCommodityData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateCommodityData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCommodityData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateCommodityData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCommodityData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateCommodityData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateCommodityData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchSalesAgentData(Object)} (Object)}
     */
    @Test
    void testFetchSalesAgentData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchSalesAgentData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchSalesAgentData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchSalesAgentData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchSalesAgentData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchSalesAgentData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchSalesAgentData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchSalesAgentData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createSalesAgentData(Object)} (Object)}
     */
    @Test
    void testCreateSalesAgentData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createSalesAgentData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateSalesAgentData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createSalesAgentData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateSalesAgentData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createSalesAgentData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateSalesAgentData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createSalesAgentData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateSalesAgentData(Object)} (Object)}
     */
    @Test
    void testUpdateSalesAgentData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateSalesAgentData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateSalesAgentData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateSalesAgentData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateSalesAgentData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateSalesAgentData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateSalesAgentData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateSalesAgentData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createOrganizationData(Object)} (Object)}
     */
    @Test
    void testCreateOrganizationData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createOrganizationData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateOrganizationData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createOrganizationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateOrganizationData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createOrganizationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateOrganizationData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createOrganizationData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchOrganization(Object)} (Object)}
     */
    @Test
    void testFetchOrganization() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchOrganization("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchOrganization2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOrganization("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchOrganization3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOrganization("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchOrganization4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOrganization("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateOrganizationData(Object)} (Object)}
     */
    @Test
    void testUpdateOrganizationData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateOrganizationData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateOrganizationData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateOrganizationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateOrganizationData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateOrganizationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateOrganizationData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateOrganizationData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createUnlocationData(Object)} (Object)}
     */
    @Test
    void testCreateUnlocationData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createUnlocationData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateUnlocationData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createUnlocationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateUnlocationData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createUnlocationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateUnlocationData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createUnlocationData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }


    /**
     * Method under test: {@link V1ServiceImpl#fetchUnlocation(Object)} (Object)}
     */
    @Test
    void testFetchUnlocation() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchUnlocation("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchUnlocation2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUnlocation("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchUnlocation3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUnlocation("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchUnlocation4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUnlocation("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateUnlocationData(Object)} (Object)}
     */
    @Test
    void testUpdateUnlocationData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateUnlocationData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateUnlocationData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateUnlocationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateUnlocationData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateUnlocationData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateUnlocationData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateUnlocationData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchMultipleMasterData(Object)} (Object)}
     */
    @Test
    void testFetchMultipleMasterData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchMultipleMasterData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchMultipleMasterData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchMultipleMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchMultipleMasterData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchMultipleMasterData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchMultipleMasterData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchMultipleMasterData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchUsersData(Object)} (Object)}
     */
    @Test
    void testFetchUsersData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchUsersData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchUsersData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUsersData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchUsersData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUsersData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchUsersData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUsersData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchGridColorCodeData(Object)} (Object)}
     */
    @Test
    void testFetchGridColorCodeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchGridColorCodeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchGridColorCodeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchGridColorCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchGridColorCodeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchGridColorCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchGridColorCodeData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchGridColorCodeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createGridColorCodeData(Object)} (Object)}
     */
    @Test
    void testCreateGridColorCodeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.createGridColorCodeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testCreateGridColorCodeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createGridColorCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateGridColorCodeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createGridColorCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCreateGridColorCodeData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createGridColorCodeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#updateGridColorCodeData(Object)} (Object)}
     */
    @Test
    void testUpdateGridColorCodeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.updateGridColorCodeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testUpdateGridColorCodeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateGridColorCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateGridColorCodeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateGridColorCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testUpdateGridColorCodeData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateGridColorCodeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#listCousinBranches(Object)} (Object)}
     */
    @Test
    void testListCousinBranches() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.listCousinBranches("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testListCousinBranches2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listCousinBranches("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testListCousinBranches3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listCousinBranches("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testListCousinBranches4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listCousinBranches("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#listCousinBranchesWithoutCurrent(Object)} (Object)}
     */
    @Test
    void testListCousinBranchesWithoutCurrent() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.listCousinBranchesWithoutCurrent("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testListCousinBranchesWithoutCurrent2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listCousinBranchesWithoutCurrent("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testListCousinBranchesWithoutCurrent3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listCousinBranchesWithoutCurrent("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testListCousinBranchesWithoutCurrent4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listCousinBranchesWithoutCurrent("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#tenantByGuid(Object)} (Object)}
     */
    @Test
    void testTenantByGuid() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        TenantIdResponse tenantIdResponse = new TenantIdResponse();
        tenantIdResponse.setId(1);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(tenantIdResponse));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.tenantByGuid("Request");
        // Assert
        assertEquals(1L, responseEntity.getId());
    }

    @Test
    void testTenantByGuid2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.tenantByGuid("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testTenantByGuid3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.tenantByGuid("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testTenantByGuid4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.tenantByGuid("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }


    /**
     * Method under test: {@link V1ServiceImpl#sendConsolidationTask(CreateConsolidationTaskRequest)} (Object)}
     */
    @Test
    void testSendConsolidationTask() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new SendEntityResponse();
        mockResponse.setIsCreated(true);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.sendConsolidationTask(CreateConsolidationTaskRequest.builder().build());
        // Assert
        assertTrue(responseEntity.getIsCreated());
    }

    @Test
    void testSendConsolidationTask2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendConsolidationTask(CreateConsolidationTaskRequest.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendConsolidationTask3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendConsolidationTask(CreateConsolidationTaskRequest.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendConsolidationTask4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendConsolidationTask(CreateConsolidationTaskRequest.builder().build()));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#sendShipmentTask(CreateShipmentTaskRequest)} (Object)}
     */
    @Test
    void testSendShipmentTask() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new SendEntityResponse();
        mockResponse.setIsCreated(true);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(mockResponse);
        // Act
        var responseEntity = v1ServiceImpl.sendShipmentTask(CreateShipmentTaskRequest.builder().build());
        // Assert
        assertTrue(responseEntity.getIsCreated());
    }

    @Test
    void testSendShipmentTask2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendShipmentTask(CreateShipmentTaskRequest.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendShipmentTask3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendShipmentTask(CreateShipmentTaskRequest.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendShipmentTask4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendShipmentTask(CreateShipmentTaskRequest.builder().build()));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request)} (Object)}
     */
    @Test
    void testSendV1ConsolidationTask() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new SendEntityResponse();
        mockResponse.setIsCreated(true);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(mockResponse);
        // Act
        var responseEntity = v1ServiceImpl.sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request.builder().build());
        // Assert
        assertTrue(responseEntity.getIsCreated());
    }

    @Test
    void testSendV1ConsolidationTask2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendV1ConsolidationTask3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendV1ConsolidationTask4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendV1ConsolidationTask(CreateV1ConsolidationTaskFromV2Request.builder().build()));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request)} (Object)}
     */
    @Test
    void testSendV1ShipmentTask() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new SendEntityResponse();
        mockResponse.setIsCreated(true);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(mockResponse);
        // Act
        var responseEntity = v1ServiceImpl.sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request.builder().build());
        // Assert
        assertTrue(responseEntity.getIsCreated());
    }

    @Test
    void testSendV1ShipmentTask2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendV1ShipmentTask3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testSendV1ShipmentTask4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.sendV1ShipmentTask(CreateV1ShipmentTaskFromV2Request.builder().build()));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#checkTaskExist(CheckTaskExistV1Request)} (Object)}
     */
    @Test
    void testCheckTaskExist() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new CheckTaskExistResponse();
        mockResponse.setSendToOrg(List.of("DPW"));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.checkTaskExist(CheckTaskExistV1Request.builder().build());
        // Assert
        assertEquals(List.of("DPW"), responseEntity.getSendToOrg());
    }

    @Test
    void testCheckTaskExist2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkTaskExist(CheckTaskExistV1Request.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCheckTaskExist3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkTaskExist(CheckTaskExistV1Request.builder().build()));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testCheckTaskExist4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkTaskExist(CheckTaskExistV1Request.builder().build()));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }


    /**
     * Method under test: {@link V1ServiceImpl#importFlightSchedules(Object)} (Object)}
     */
    @Test
    void testImportFlightSchedules() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.importFlightSchedules("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testImportFlightSchedules2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.importFlightSchedules("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testImportFlightSchedules3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.importFlightSchedules("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchFlightStatus(Object)} (Object)}
     */
    @Test
    void testFetchFlightStatus() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchFlightStatus("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchFlightStatus2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchFlightStatus("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchFlightStatus3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchFlightStatus("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#importSailingSchedules(Object)} (Object)}
     */
    @Test
    void testImportSailingSchedules() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.importSailingSchedules("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testImportSailingSchedules2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.importSailingSchedules("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testImportSailingSchedules3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.importSailingSchedules("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#listSailingSchedule(Object)} (Object)}
     */
    @Test
    void testListSailingSchedule() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.listSailingSchedule("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testListSailingSchedule2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listSailingSchedule("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testListSailingSchedule3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listSailingSchedule("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#addressList(Object)} (Object)}
     */
    @Test
    void testAddressList() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.addressList("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testAddressList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.addressList("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testAddressList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.addressList("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#tenantNameByTenantId(Object)} (Object)}
     */
    @Test
    void testTenantNameByTenantId() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.tenantNameByTenantId("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testTenantNameByTenantId2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.tenantNameByTenantId("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testTenantNameByTenantId3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.tenantNameByTenantId("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchChargeCodeData(Object)} (Object)}
     */
    @Test
    void testFetchChargeCodeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchChargeCodeData("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchChargeCodeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchChargeCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchChargeCodeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchChargeCodeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#retrieveChargeCodeData(Object)} (Object)}
     */
    @Test
    void testRetrieveChargeCodeData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1RetrieveResponse.builder().entity("1L").build()));
        when(mock.getBody()).thenReturn(V1RetrieveResponse.builder().entity("1L").build());
        // Act
        var responseEntity = v1ServiceImpl.retrieveChargeCodeData("Request");
        // Assert
        assertEquals("1L", responseEntity.getEntity());
    }

    @Test
    void testRetrieveChargeCodeData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveChargeCodeData("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testRetrieveChargeCodeData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveChargeCodeData("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchUnlocationOriginAndDestinationList(Object)} (Object)}
     */
    @Test
    void testFetchUnlocationOriginAndDestinationList() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchUnlocationOriginAndDestinationList("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchUnlocationOriginAndDestinationList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUnlocationOriginAndDestinationList("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchUnlocationOriginAndDestinationList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchUnlocationOriginAndDestinationList("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchListUnlocationTransportModeBased(Object)} (Object)}
     */
    @Test
    void testfetchListUnlocationTransportModeBased() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchListUnlocationTransportModeBased("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testfetchListUnlocationTransportModeBased2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchListUnlocationTransportModeBased("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testfetchListUnlocationTransportModeBased3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchListUnlocationTransportModeBased("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchActivityMaster(Object)} (Object)}
     */
    @Test
    void testFetchActivityMaster() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchActivityMaster("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchActivityMaster2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchActivityMaster("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchActivityMaster3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchActivityMaster("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#retrieveTenantSettings(Object)} (Object)}
     */
    @Test
    void testRetrieveTenantSettings() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1RetrieveResponse.builder().entity("1L").build()));
        when(mock.getBody()).thenReturn(V1RetrieveResponse.builder().entity("1L").build());
        // Act
        var responseEntity = v1ServiceImpl.retrieveTenantSettings();
        // Assert
        assertEquals("1L", responseEntity.getEntity());
    }

    @Test
    void testRetrieveTenantSettings2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveTenantSettings());
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testRetrieveTenantSettings3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveTenantSettings());
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

}
