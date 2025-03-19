package com.dpw.runner.shipment.services.service.v1.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.dto.request.UserWithPermissionRequestV1;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.impl.GetUserServiceFactory;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.TokenUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

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

    @MockBean
    private CommonUtils commonUtils;


    @MockBean
    private GetUserServiceFactory getUserServiceFactory;

    @MockBean
    private TokenUtility tokenUtility;

    @MockBean
    private CacheManager cacheManager;

    private V1UsersEmailRequest request;
    private UsersRoleListResponse userRole;

    @BeforeAll
    static void init() throws IOException {
        objectMapper = JsonTestUtility.getMapper();
        v1ErrorResponse = V1ErrorResponse.builder().error(V1ErrorResponse.V1Error.builder().message(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE).build()).build();
        v1ErrorInString = objectMapper.writeValueAsString(v1ErrorInString);
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);

        request = new V1UsersEmailRequest();
        request.setRoleId(1);
        request.setTake(10);

        userRole = UsersRoleListResponse.builder()
                .roleId(1L)
                .userName("John Doe")
                .email("john.doe@example.com")
                .build();
    }

    @Test
    void testGetTenantName_WithValidTenantIds_ReturnsTenantNames() {
        // Arrange
        List<Integer> tenantIds = List.of(1, 2, 3);

        CommonV1ListRequest expectedRequest = new CommonV1ListRequest();
        expectedRequest.setCriteriaRequests(List.of(
                List.of(Constants.TENANTID),
                Operators.IN.getValue(),
                List.of(tenantIds)
        ));

        V1DataResponse tenantNameResponse = new V1DataResponse();
        tenantNameResponse.entities = List.of(
                new V1TenantResponse("Tenant 1", "T1", 1L),
                new V1TenantResponse("Tenant 2", "T2", 2L)
        );

        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(tenantNameResponse));


        when(jsonHelper.convertValueToList(tenantNameResponse.entities, V1TenantResponse.class))
                .thenReturn(List.of(new V1TenantResponse("Tenant 1", "T1", 1L), new V1TenantResponse("Tenant 2", "T2", 2L)));

        // Act
        List<String> tenantNames = v1ServiceImpl.getTenantName(tenantIds);

        // Assert
        assertNotNull(tenantNames);
        assertEquals(2, tenantNames.size());
        assertEquals("Tenant 1", tenantNames.get(0));
        assertEquals("Tenant 2", tenantNames.get(1));
    }

    @Test
    void testGetTenantName_WithValidTenantIds_ReturnsTenantNames_nullTenantResponse() {
        // Arrange
        List<Integer> tenantIds = List.of(1, 2, 3);

        CommonV1ListRequest expectedRequest = new CommonV1ListRequest();
        expectedRequest.setCriteriaRequests(List.of(
                List.of(Constants.TENANTID),
                Operators.IN.getValue(),
                List.of(tenantIds)
        ));

        V1DataResponse tenantNameResponse = new V1DataResponse();
        tenantNameResponse.entities = List.of(
                new V1TenantResponse("Tenant 1", "T1", 1L),
                new V1TenantResponse("Tenant 2", "T2", 2L)
        );

        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(tenantNameResponse));


        when(jsonHelper.convertValueToList(tenantNameResponse.entities, V1TenantResponse.class))
                .thenReturn(null);

        // Act
        List<String> tenantNames = v1ServiceImpl.getTenantName(tenantIds);

        // Assert
        assertNotNull(tenantNames);
        assertEquals(0, tenantNames.size());
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
     * Method under test: {@link V1ServiceImpl#stateBasedList(Object)} (Object)}
     */
    @Test
    void testStateBasedList() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.stateBasedList("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testStateBasedList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.stateBasedList("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testStateBasedList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.stateBasedList("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testStateBasedList4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.stateBasedList("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    @Test
    void testStateBasedList5() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(null));
        when(mock.getBody()).thenReturn(null);
        // Act
        var responseEntity = v1ServiceImpl.stateBasedList("Request");
        // Assert
        assertNull(responseEntity.getEntities());
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
     * Method under test: {@link V1ServiceImpl#retrieveTenantSettings()}
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

    /**
     * Method under test: {@link V1ServiceImpl#retrieveCompanySettings()} )}
     */
    @Test
    void testRetrieveCompanySettings() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(CompanySettingsResponse.builder().SeaLclContainerFlag(true).build()));
        when(mock.getBody()).thenReturn(CompanySettingsResponse.builder().SeaLclContainerFlag(true).build());
        // Act
        var responseEntity = v1ServiceImpl.retrieveCompanySettings();
        // Assert
        assertTrue(responseEntity.getSeaLclContainerFlag());
    }

    @Test
    void testRetrieveCompanySettings2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveCompanySettings());
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testRetrieveCompanySettings3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveCompanySettings());
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }


    /**
     * Method under test: {@link V1ServiceImpl#getDefaultOrg()} )}
     */
    @Test
    void testGetDefaultOrg() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new PartyRequestV2();
        mockResponse.setOrgCode("DPW");
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(mockResponse);
        // Act
        var responseEntity = v1ServiceImpl.getDefaultOrg();
        // Assert
        assertEquals("DPW", responseEntity.getOrgCode());
    }

    @Test
    void testGetDefaultOrg_OrgData() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new PartyRequestV2();
        mockResponse.setOrgCode("DPW");
        mockResponse.setOrgData(Map.of("Id", 213));
        mockResponse.setAddressData(Map.of("Id", 432));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(mockResponse);
        // Act
        var responseEntity = v1ServiceImpl.getDefaultOrg();
        // Assert
        assertEquals("DPW", responseEntity.getOrgCode());
    }

    @Test
    void testGetDefaultOrg_OrgData_Without_Id() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        var mockResponse = new PartyRequestV2();
        mockResponse.setOrgCode("DPW");
        mockResponse.setOrgData(Map.of());
        mockResponse.setAddressData(Map.of());
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(mock.getBody()).thenReturn(mockResponse);
        // Act
        var responseEntity = v1ServiceImpl.getDefaultOrg();
        // Assert
        assertEquals("DPW", responseEntity.getOrgCode());
    }

    @Test
    void testGetDefaultOrg2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getDefaultOrg());
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testGetDefaultOrg3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getDefaultOrg());
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchOwnType(Object)} (Object)}
     */
    @Test
    void testFetchOwnType() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchOwnType("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchOwnType2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOwnType("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchOwnType3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOwnType("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchCarrierFilterList(Object)} (Object)}
     */
    @Test
    void testFetchCarrierFilterList() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        when(mock.getBody()).thenReturn(V1DataResponse.builder().entityId(1L).build());
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierFilterList("Request");
        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierFilterList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCarrierFilterList("Request"));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchCarrierFilterList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCarrierFilterList("Request"));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }


    /**
     * Method under test: {@link V1ServiceImpl#retrieveTenant()}
     */
    @Test
    void testRetrieveTenant() throws RestClientException {
        // Arrange
        var mock = mock(ResponseEntity.class);
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1RetrieveResponse.builder().entity("1L").build()));
        when(mock.getBody()).thenReturn(V1RetrieveResponse.builder().entity("1L").build());
        // Act
        var responseEntity = v1ServiceImpl.retrieveTenant();
        // Assert
        assertEquals("1L", responseEntity.getEntity());
    }

    @Test
    void testRetrieveTenant2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveTenant());
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testRetrieveTenant3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveTenant());
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchTransportInstructionList(Object)}
     */
    @Test
    void testFetchTransportInstructionList() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        // Act
        var responseEntity = v1ServiceImpl.fetchTransportInstructionList(CheckCreditLimitResponse.builder().build());

        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchTransportInstructionList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchTransportInstructionList(CheckCreditLimitResponse.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchTransportInstructionList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchTransportInstructionList(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchTransportInstructionList4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchTransportInstructionList(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchContainersListForTI(Object)}
     */
    @Test
    void testFetchContainersListForTI() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(V1DataResponse.builder().entityId(1L).build()));
        // Act
        var responseEntity = v1ServiceImpl.fetchContainersListForTI(CheckCreditLimitResponse.builder().build());

        // Assert
        assertEquals(1L, responseEntity.getEntityId());
    }

    @Test
    void testFetchContainersListForTI2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchContainersListForTI(CheckCreditLimitResponse.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchContainersListForTI3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchContainersListForTI(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchContainersListForTI4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchContainersListForTI(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchConsolidationBookingData(Object)}
     */
    @Test
    void testFetchConsolidationBookingData() throws RestClientException {
        var mockResponse = new ConsoleBookingListResponse();
        var hashMap = new HashMap<UUID, ConsoleBookingListResponse.BookingData>();
        hashMap.put(UUID.randomUUID(), new ConsoleBookingListResponse.BookingData());
        mockResponse.setData(hashMap);
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.fetchConsolidationBookingData(CheckCreditLimitResponse.builder().build());

        // Assert
        assertEquals(hashMap, responseEntity.getData());
    }

    @Test
    void testFetchConsolidationBookingData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchConsolidationBookingData(CheckCreditLimitResponse.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchConsolidationBookingData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchConsolidationBookingData(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchConsolidationBookingData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchConsolidationBookingData(CheckCreditLimitResponse.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchShipmentBillingData(Object)}
     */
    @Test
    void testFetchShipmentBillingData() throws RestClientException {
        var mockResponse = new ShipmentBillingListResponse();
        var hashMap = new HashMap<String, ShipmentBillingListResponse.BillingData>();
        hashMap.put(UUID.randomUUID().toString(), new ShipmentBillingListResponse.BillingData());
        mockResponse.setData(hashMap);
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.fetchShipmentBillingData(ShipmentBillingListRequest.builder().build());

        // Assert
        assertEquals(hashMap, responseEntity.getData());
    }

    @Test
    void testFetchShipmentBillingData2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchShipmentBillingData(ShipmentBillingListRequest.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchShipmentBillingData3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchShipmentBillingData(ShipmentBillingListRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchShipmentBillingData4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchShipmentBillingData(ShipmentBillingListRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchWayBillNumberFilterGuids(Object)}
     */
    @Test
    void testfetchWayBillNumberFilterGuids() throws RestClientException {
        var mockResponse = new GuidsListResponse();
        List<UUID> guidsList = List.of(UUID.randomUUID());
        mockResponse.setGuidsList(guidsList);
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.fetchWayBillNumberFilterGuids("Request");

        // Assert
        assertEquals(guidsList, responseEntity.getGuidsList());
    }

    @Test
    void testfetchWayBillNumberFilterGuids2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchWayBillNumberFilterGuids("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testfetchWayBillNumberFilterGuids3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchWayBillNumberFilterGuids("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testfetchWayBillNumberFilterGuids4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchWayBillNumberFilterGuids("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchBookingIdFilterGuids(Object)}
     */
    @Test
    void testFetchBookingIdFilterGuids() throws RestClientException {
        var mockResponse = new GuidsListResponse();
        var guidsList = List.of(UUID.randomUUID());
        mockResponse.setGuidsList(guidsList);
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchBookingIdFilterGuids("Request");

        // Assert
        assertEquals(guidsList, responseEntity.getGuidsList());
    }

    @Test
    void testFetchBookingIdFilterGuids2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBookingIdFilterGuids("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchBookingIdFilterGuids3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBookingIdFilterGuids("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchBookingIdFilterGuids4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBookingIdFilterGuids("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createTaskforHBL(Object)}
     */
    @Test
    void testCreateTaskforHBL() throws RestClientException {
        var mockResponse = new HblTaskCreationResponse();
        mockResponse.setIsCreated(true);
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.createTaskforHBL("Request");

        // Assert
        assertTrue(responseEntity.getIsCreated());
    }

    @Test
    void testCreateTaskforHBL2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createTaskforHBL("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testCreateTaskforHBL3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createTaskforHBL("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testCreateTaskforHBL4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createTaskforHBL("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchGetTemplateMainPage(Object)}
     */
    @Test
    void testFetchGetTemplateMainPage() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchGetTemplateMainPage("Request");

        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchGetTemplateMainPage2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchGetTemplateMainPage("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchGetTemplateMainPage3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchGetTemplateMainPage("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchGetTemplateMainPage4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchGetTemplateMainPage("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchRolesList(Object)}
     */
    @Test
    void testFetchRolesList() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchRolesList("Request");

        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchRolesList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchRolesList("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchRolesList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchRolesList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchRolesList4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchRolesList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchBillingList(Object)}
     */
    @Test
    void testFetchBillingList() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchBillingList("Request");

        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchBillingList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBillingList("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchBillingList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBillingList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchBillingList4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBillingList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchBillChargesList(Object)}
     */
    @Test
    void testFetchBillChargesList() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchBillChargesList("Request");

        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchBillChargesList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBillChargesList("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchBillChargesList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBillChargesList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchBillChargesList4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchBillChargesList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchArObjectList(Object)}
     */
    @Test
    void testFetchArObjectList() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchArObjectList("Request");

        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchArObjectList2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchArObjectList("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchArObjectList3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchArObjectList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchArObjectList4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchArObjectList("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchCreditLimit(Object)}
     */
    @Test
    void testFetchCreditLimit() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchCreditLimit("Request");

        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCreditLimit2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCreditLimit("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchCreditLimit3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCreditLimit("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchCreditLimit4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCreditLimit("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#getShipment(V1RetrieveRequest)}
     */
    @Test
    void testGetShipment() throws RestClientException {
        var mockResponse = V1RetrieveResponse.builder().entity(1L).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.getShipment(V1RetrieveRequest.builder().build());

        // Assert
        assertEquals(mockResponse.getEntity(), responseEntity.getEntity());
    }

    @Test
    void testGetShipment2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getShipment(V1RetrieveRequest.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetShipment3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getShipment(V1RetrieveRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetShipment4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getShipment(V1RetrieveRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#checkCreditLimit(CreditLimitValidateRequest)}
     */
    @Test
    void testCheckCreditLimit() throws RestClientException {
        var mockResponse = CreditLimitValidateResponse.builder().isValid(true).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.checkCreditLimit(CreditLimitValidateRequest.builder().build());

        // Assert
        assertEquals(mockResponse.getIsValid(), responseEntity.getIsValid());
    }

    @Test
    void testCheckCreditLimit2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkCreditLimit(CreditLimitValidateRequest.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testCheckCreditLimit3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(CreditLimitValidateResponse.class))).thenReturn(CreditLimitValidateResponse.builder().error(V1ErrorResponse.V1Error.builder().message(v1ErrorInString).build()).build());
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkCreditLimit(CreditLimitValidateRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testCheckCreditLimit4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.FORBIDDEN));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkCreditLimit(CreditLimitValidateRequest.builder().build()));
        assertEquals(HttpClientErrorException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testCheckCreditLimit5() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.checkCreditLimit(CreditLimitValidateRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#getAddressTranslation(AddressTranslationRequest)}
     */
    @Test
    void testGetAddressTranslation() throws RestClientException {
        var mockResponse = AddressTranslationListResponse.builder().addressTranslationList(List.of()).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.getAddressTranslation(AddressTranslationRequest.builder().build());

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void testGetAddressTranslation2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getAddressTranslation(AddressTranslationRequest.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetAddressTranslation3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getAddressTranslation(AddressTranslationRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetAddressTranslation4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getAddressTranslation(AddressTranslationRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#getActiveInvoices(CheckActiveInvoiceRequest)}
     */
    @Test
    void testGetActiveInvoices() throws RestClientException {
        var mockResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(true).build();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipmentLite(false).build());
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        // Act
        var responseEntity = v1ServiceImpl.getActiveInvoices(CheckActiveInvoiceRequest.builder().build());

        // Assert
        assertFalse(responseEntity.getIsAnyActiveInvoiceFound());
    }

    @Test
    void testGetActiveInvoices2() throws RestClientException {
        var mockResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(true).build();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipmentLite(true).build());
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        // Act
        var responseEntity = v1ServiceImpl.getActiveInvoices(CheckActiveInvoiceRequest.builder().build());

        // Assert
        assertEquals(mockResponse.getIsAnyActiveInvoiceFound(), responseEntity.getIsAnyActiveInvoiceFound());
    }

    @Test
    void testGetActiveInvoices3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getActiveInvoices(CheckActiveInvoiceRequest.builder().build()));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetActiveInvoices4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getActiveInvoices(CheckActiveInvoiceRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetActiveInvoices5() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getActiveInvoices(CheckActiveInvoiceRequest.builder().build()));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchOrgAddresses(Object)}
     */
    @Test
    void testFetchOrgAddresses() throws RestClientException {
        var mockResponse = OrgAddressResponse.builder().organizations(new HashMap<>()).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));

        // Act
        var responseEntity = v1ServiceImpl.fetchOrgAddresses("Request");

        // Assert
        assertEquals(mockResponse.getOrganizations(), responseEntity.getOrganizations());
    }

    @Test
    void testFetchOrgAddresses2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOrgAddresses("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchOrgAddresses3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOrgAddresses("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchOrgAddresses4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchOrgAddresses("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchAddress(String)}
     */
    @Test
    void testFetchAddress() throws RestClientException {
        var inputEntity = new EntityTransferAddress();
        inputEntity.setOrgId(111L);
        var mockResponse = V1RetrieveResponse.builder().entity(inputEntity).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(modelMapper.map(any(), eq(EntityTransferAddress.class))).thenReturn(inputEntity);
        // Act
        var responseEntity = v1ServiceImpl.fetchAddress("Request");

        // Assert
        assertEquals(inputEntity.getOrgId(), responseEntity.getOrgId());
    }

    @Test
    void testFetchAddress2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchAddress("Request"));
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchAddress3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));

        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchAddress("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testFetchAddress4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchAddress("Request"));
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#fetchCarrierMasterData(Object, boolean)} ()}
     */
    @Test
    void testFetchCarrierMasterData() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, false);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData2() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        var request = new CarrierListObject();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, v1ErrorInString));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCarrierMasterData(mockResponse, false));
        // Assert
        assertEquals(v1ErrorResponse.getError().getMessage(), throwable.getMessage());
    }

    @Test
    void testFetchCarrierMasterData3() throws RestClientException {
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        var request = new CarrierListObject();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        Throwable throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.fetchCarrierMasterData(mockResponse, false));
        // Assert
        assertEquals("RuntimeException", throwable.getMessage());
    }

    @Test
    void testFetchCarrierMasterData4() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setType(Constants.CONSOLIDATION_TYPE_AGT);
        request.setIsList(true);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData5() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setListObject("Criteria");
        request.setType(Constants.CONSOLIDATION_TYPE_AGT);
        request.setIsList(false);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData6() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setListObject("Criteria");
        request.setType(Constants.SHIPMENT_TYPE_DRT);
        request.setIsList(true);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData7() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setListObject("Criteria");
        request.setIsList(true);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData8() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setIsList(false);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData9() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setType(Constants.CONSOLIDATION_TYPE_CLD);
        request.setIsList(true);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData10() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setType(Constants.CONSOLIDATION_TYPE_CLD);
        request.setIsList(false);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }

    @Test
    void testFetchCarrierMasterData11() throws RestClientException {
        // Arrange
        var request = new CarrierListObject();
        request.setType(Constants.SHIPMENT_TYPE_DRT);
        request.setIsList(null);
        var mockResponse = V1DataResponse.builder().entityId(87L).build();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(jsonHelper.convertValue(any(), eq(CarrierListObject.class))).thenReturn(request);
        // Act
        var responseEntity = v1ServiceImpl.fetchCarrierMasterData(request, true);
        // Assert
        assertEquals(mockResponse.getEntityId(), responseEntity.getEntityId());
    }


    /**
     * Method under test: {@link V1ServiceImpl#v1DataSync(Object, HttpHeaders)}
     */
    @Test
    void testV1DataSync() throws RestClientException {
        var mockResponse = V1DataSyncResponse.builder().isSuccess(true).build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(v1AuthHelper.getHeadersForDataSync()).thenReturn(new HttpHeaders());
        // Act
        var responseEntity = v1ServiceImpl.v1DataSync("Request", null);

        // Assert
        assertTrue(responseEntity.getIsSuccess());
    }

    @Test
    void testV1DataSync2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));

        // Act and Assert
        var responseEntity = v1ServiceImpl.v1DataSync("Request", new HttpHeaders());

        // Assert
        assertFalse(responseEntity.getIsSuccess());
    }

    /**
     * Method under test: {@link V1ServiceImpl#createBooking(CustomerBooking, boolean, boolean, UUID, HttpHeaders)}
     */
    @Test
    void testCreateBooking() throws RestClientException {
        var mockResponse = ResponseEntity.ok(new V1ShipmentCreationResponse());
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(), any())).thenReturn(CreateBookingModuleInV1.builder().IsP100Booking(true).Entity(new CreateBookingModuleInV1.BookingEntity()).build());
        // Act
        var responseEntity = v1ServiceImpl.createBooking(CustomerBooking.builder().build(), false, false, UUID.randomUUID(), null);

        // Assert
        assertEquals(mockResponse.getStatusCode() , responseEntity.getStatusCode());
    }

    @Test
    void testCreateBooking2() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act and Assert
        Throwable t = assertThrows(Throwable.class, () -> v1ServiceImpl.createBooking(CustomerBooking.builder().build(), false, false, UUID.randomUUID(), null));
        // Assert
        assertEquals(V1ServiceException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreateBooking3() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpServerErrorException(HttpStatus.UNAUTHORIZED, v1ErrorInString));
        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class))).thenReturn(v1ErrorResponse);
        // Act and Assert
        Throwable t = assertThrows(Throwable.class, () -> v1ServiceImpl.createBooking(CustomerBooking.builder().build(), false, false, UUID.randomUUID(), new HttpHeaders()));
        // Assert
        assertEquals(V1ServiceException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreateBooking4() throws RestClientException {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException(v1ErrorInString));
        // Act and Assert
        Throwable t = assertThrows(Throwable.class, () -> v1ServiceImpl.createBooking(CustomerBooking.builder().build(), false, false, UUID.randomUUID(), new HttpHeaders()));
        // Assert
        assertEquals(V1ServiceException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#getMaxConsolidationId()}
     */
    @Test
    void testGetMaxConsolidationId() throws RestClientException {
        var mockResponse = "10101";
        // Arrange
        ResponseEntity<Object> responseEntity = new ResponseEntity<>(mockResponse, HttpStatus.OK);
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenReturn(responseEntity);
        // Act
        var actualEntity = v1ServiceImpl.getMaxConsolidationId();
        // Assert
        assertEquals(mockResponse, actualEntity);
    }

    @Test
    void testGetMaxConsolidationId2() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMaxConsolidationId());
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetMaxConsolidationId3() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMaxConsolidationId());
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetMaxConsolidationId4() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new RuntimeException("Runtime"));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMaxConsolidationId());
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#getShipmentSerialNumber()}
     */
    @Test
    void testGetShipmentSerialNumber() throws RestClientException {
        var mockResponse = "10101";
        // Arrange
        ResponseEntity<Object> responseEntity = new ResponseEntity<>(mockResponse, HttpStatus.OK);
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenReturn(responseEntity);
        // Act
        var actualEntity = v1ServiceImpl.getShipmentSerialNumber();
        // Assert
        assertEquals(mockResponse, actualEntity);
    }

    @Test
    void testGetShipmentSerialNumber2() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getShipmentSerialNumber());
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetShipmentSerialNumber3() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getShipmentSerialNumber());
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetShipmentSerialNumber4() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new RuntimeException("Runtime"));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getShipmentSerialNumber());
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    /**
     * Method under test: {@link V1ServiceImpl#getMaxShipmentId()}
     */
    @Test
    void testGetMaxShipmentId() throws RestClientException {
        var mockResponse = "10101";
        // Arrange
        ResponseEntity<Object> responseEntity = new ResponseEntity<>(mockResponse, HttpStatus.OK);
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenReturn(responseEntity);
        // Act
        var actualEntity = v1ServiceImpl.getMaxShipmentId();
        // Assert
        assertEquals(mockResponse, actualEntity);
    }

    @Test
    void testGetMaxShipmentId2() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMaxShipmentId());
        assertEquals(UnAuthorizedException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetMaxShipmentId3() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));
        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMaxShipmentId());
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetMaxShipmentId4() throws RestClientException {
        // Arrange
        when(restTemplate.exchange(anyString(), any(), any(), eq(Object.class)))
                .thenThrow(new RuntimeException("Runtime"));

        // Act and Assert
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMaxShipmentId());
        assertEquals(V1ServiceException.class.getSimpleName(), throwable.getClass().getSimpleName());
    }

    @Test
    void testGetCoLoadingStations() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
            (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.getCoLoadingStations("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void testGetCoLoadingStationsThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
            (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getCoLoadingStations("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void testGetCoLoadingStationsThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
            (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.getCoLoadingStations("Request"));

        // Assert
        assertNotNull(throwable);
    }


    @Test
    void getTenantDetails() {
        var mockResponse = TenantDetailsByListResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.getTenantDetails("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getTenantDetailsThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getTenantDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getTenantDetailsThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.getTenantDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getEmailTemplates() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.getEmailTemplates("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getEmailTemplatesThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getEmailTemplates("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getEmailTemplatesThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.getEmailTemplates("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getMasterDetails() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.getMasterDetails("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getMasterDetailsThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getMasterDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getMasterDetailsThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.getMasterDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getUserDetails() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.getUserDetails("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getUserDetailsThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getUserDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getUserDetailsThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.getUserDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void testGetUserEmailsByRoleId_Success() {
        List<UsersRoleListResponse> responseList = Arrays.asList(userRole);
        ResponseEntity<List<UsersRoleListResponse>> responseEntity = ResponseEntity.ok(responseList);

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        List<UsersRoleListResponse> result = v1ServiceImpl.getUserEmailsByRoleId(request);

        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("john.doe@example.com", result.get(0).getEmail());
    }

    @Test
    void testGetUserEmailsByRoleId_HttpClientErrorException() {
        HttpClientErrorException ex = mock(HttpClientErrorException.class);
        when(ex.getResponseBodyAsString()).thenReturn("{\"error\": {\"message\": \"Client Error\"}}");

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenThrow(ex);

        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class)))
                .thenReturn(new V1ErrorResponse(new V1ErrorResponse.V1Error("Client Error")));

        V1ServiceException thrown = assertThrows(V1ServiceException.class, () -> {
            v1ServiceImpl.getUserEmailsByRoleId(request);
        });

        assertEquals("Client Error", thrown.getMessage());
    }

    @Test
    void testGetUserEmailsByRoleId_HttpServerErrorException() {
        HttpServerErrorException ex = mock(HttpServerErrorException.class);
        when(ex.getResponseBodyAsString()).thenReturn("{\"error\": {\"message\": \"Server Error\"}}");

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenThrow(ex);

        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class)))
                .thenReturn(new V1ErrorResponse(new V1ErrorResponse.V1Error("Server Error")));

        V1ServiceException thrown = assertThrows(V1ServiceException.class, () -> {
            v1ServiceImpl.getUserEmailsByRoleId(request);
        });

        assertEquals("Server Error", thrown.getMessage());
    }


    @Test
    void testGetUserEmailsByRoleId_GeneralException() {
        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenThrow(new RuntimeException("General Error"));

        V1ServiceException thrown = assertThrows(V1ServiceException.class, () -> {
            v1ServiceImpl.getUserEmailsByRoleId(request);
        });

        assertEquals("General Error", thrown.getMessage());
    }

    @Test
    void updateTask() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.updateTask("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void updateTask2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.updateTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void updateTask3() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.updateTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void retrieveTask() {
        var mockResponse = V1RetrieveResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.retrieveTask("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void retrieveTask2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.retrieveTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void retrieveTask3() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.retrieveTask("Request"));

        // Assert
        assertNotNull(throwable);
    }


    @Test
    void listTask() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.listTask("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void listTask2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void listTask3() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.listTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void createTask() {
        var mockResponse = TaskCreateResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.createTask("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void createTask2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.createTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void createTask3() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.createTask("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void testClearAuthContext() {
        // Mocking the static methods using Mockito
        try (MockedStatic<TenantContext> tenantContextMock = mockStatic(TenantContext.class);
                MockedStatic<RequestAuthContext> authContextMock = mockStatic(RequestAuthContext.class);
                MockedStatic<PermissionsContext> permissionsContextMock = mockStatic(PermissionsContext.class);
                MockedStatic<SecurityContextHolder> securityContextHolderMock = mockStatic(SecurityContextHolder.class);
                MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {

            // Call the method to be tested
            v1ServiceImpl.clearAuthContext();

            // Verify the method calls
            tenantContextMock.verify(TenantContext::removeTenant, times(1));
            authContextMock.verify(RequestAuthContext::removeToken, times(1));
            permissionsContextMock.verify(PermissionsContext::removePermissions, times(1));
            securityContextHolderMock.verify(SecurityContextHolder::clearContext, times(1));
            userContextMock.verify(UserContext::removeUser, times(1));
        }
    }

    @Test
    void testGetAuthoritiesWithNoPermissions() {
        // Test with an empty list
        List<String> permissions = Collections.emptyList();
        Collection<? extends GrantedAuthority> authorities = v1ServiceImpl.getAuthorities(permissions);

        // Verify that the returned collection is empty
        assertTrue(authorities.isEmpty(), "The authorities collection should be empty.");
    }

    @Test
    void testGetAuthoritiesWithPermissions() {
        // Test with a list of permissions
        List<String> permissions = Arrays.asList("READ", "WRITE", "DELETE");
        Collection<? extends GrantedAuthority> authorities = v1ServiceImpl.getAuthorities(permissions);

        // Verify that the correct number of authorities are returned
        assertEquals(3, authorities.size(), "The authorities collection should contain three elements.");

        // Verify that the authorities contain the expected values
        assertTrue(authorities.contains(new SimpleGrantedAuthority("READ")), "The authorities should contain READ permission.");
        assertTrue(authorities.contains(new SimpleGrantedAuthority("WRITE")), "The authorities should contain WRITE permission.");
        assertTrue(authorities.contains(new SimpleGrantedAuthority("DELETE")), "The authorities should contain DELETE permission.");
    }

    @Test
    void testGetAuthoritiesWithDuplicatePermissions() {
        // Test with duplicate permissions
        List<String> permissions = Arrays.asList("READ", "WRITE", "READ"); // Duplicate READ
        Collection<? extends GrantedAuthority> authorities = v1ServiceImpl.getAuthorities(permissions);

        // Verify that the correct number of authorities are returned
        assertEquals(3, authorities.size(), "The authorities collection should contain three elements.");

        // Verify that the authorities contain the expected values, duplicates are not an issue here
        assertTrue(authorities.contains(new SimpleGrantedAuthority("READ")), "The authorities should contain READ permission.");
        assertTrue(authorities.contains(new SimpleGrantedAuthority("WRITE")), "The authorities should contain WRITE permission.");
    }

    @Test
    void testSetAuthContext() {
        // Mocking dependencies
        String token = "Bearer sampleToken";
        String username = "user1";
        UsersDto user = mock(UsersDto.class);
        Map<String, Boolean> permissionsMap = new HashMap<>();
        permissionsMap.put("READ", true);
        permissionsMap.put("WRITE", true);

        when(user.getTenantId()).thenReturn(1);
        when(user.getUsername()).thenReturn(username);
        when(user.getPermissions()).thenReturn(permissionsMap);

        // Mocking static methods
        try (MockedStatic<RequestAuthContext> requestAuthContextMock = mockStatic(RequestAuthContext.class);
                MockedStatic<TenantContext> tenantContextMock = mockStatic(TenantContext.class);
                MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class);
                MockedStatic<SecurityContextHolder> securityContextHolderMock = mockStatic(SecurityContextHolder.class);
                MockedStatic<PermissionsContext> permissionsContextMock = mockStatic(PermissionsContext.class)) {

            // Mock the SecurityContext and ensure only one instance is used
            SecurityContext contextMock = mock(SecurityContext.class);
            securityContextHolderMock.when(SecurityContextHolder::getContext).thenReturn(contextMock);

            // Call the method to be tested
            v1ServiceImpl.setAuthContext(token, user);

            // Verify that the authentication token was set
            requestAuthContextMock.verify(() -> RequestAuthContext.setAuthToken(token), times(1));

            // Verify that the tenant context was set
            tenantContextMock.verify(() -> TenantContext.setCurrentTenant(1), times(1));

            // Verify that the user context was set
            userContextMock.verify(() -> UserContext.setUser(user), times(1));

            // Create the expected authentication token
            List<String> grantedPermissions = new ArrayList<>(permissionsMap.keySet());
            UsernamePasswordAuthenticationToken expectedAuthenticationToken =
                    new UsernamePasswordAuthenticationToken(user, null, v1ServiceImpl.getAuthorities(grantedPermissions));

            // Verify that the security context authentication was set correctly
            verify(contextMock, times(1)).setAuthentication(expectedAuthenticationToken);

            // Verify that the permissions context was set
            permissionsContextMock.verify(() -> PermissionsContext.setPermissions(grantedPermissions), times(1));
        }
    }

    @Test
    void testSetAuthContextWithEmptyPermissions() {
        // Mocking dependencies
        String token = "Bearer sampleToken";
        int tenantId = 1; // Changed to int to match the expected type
        String username = "user1";
        UsersDto user = mock(UsersDto.class);
        Map<String, Boolean> permissionsMap = new HashMap<>(); // Empty permissions

        when(user.getTenantId()).thenReturn(tenantId);
        when(user.getUsername()).thenReturn(username);
        when(user.getPermissions()).thenReturn(permissionsMap);

        // Mocking static methods
        try (MockedStatic<RequestAuthContext> requestAuthContextMock = mockStatic(RequestAuthContext.class);
                MockedStatic<TenantContext> tenantContextMock = mockStatic(TenantContext.class);
                MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class);
                MockedStatic<SecurityContextHolder> securityContextHolderMock = mockStatic(SecurityContextHolder.class);
                MockedStatic<PermissionsContext> permissionsContextMock = mockStatic(PermissionsContext.class)) {

            // Mock the SecurityContextHolder behavior
            SecurityContext contextMock = mock(SecurityContext.class);
            securityContextHolderMock.when(SecurityContextHolder::getContext).thenReturn(contextMock);

            // Call the method to be tested
            v1ServiceImpl.setAuthContext(token, user);

            // Verify that the authentication token was set
            requestAuthContextMock.verify(() -> RequestAuthContext.setAuthToken(token), times(1));

            // Verify that the tenant context was set
            tenantContextMock.verify(() -> TenantContext.setCurrentTenant(tenantId), times(1));

            // Verify that the user context was set
            userContextMock.verify(() -> UserContext.setUser(user), times(1));

            // Create the expected authentication token with empty permissions
            List<String> grantedPermissions = new ArrayList<>(permissionsMap.keySet());
            UsernamePasswordAuthenticationToken expectedAuthenticationToken =
                    new UsernamePasswordAuthenticationToken(user, null, v1ServiceImpl.getAuthorities(grantedPermissions));

            // Verify that the security context authentication was set correctly
            verify(contextMock, times(1)).setAuthentication(expectedAuthenticationToken);

            // Verify that the permissions context was set with an empty list
            permissionsContextMock.verify(() -> PermissionsContext.setPermissions(grantedPermissions), times(1));
        }
    }

    @Test
    void testGenerateTokenReturnsNullCache() {
        // Mock the token generation API response

        Cache mockCache = null;
        when(cacheManager.getCache(anyString())).thenReturn(mockCache);

        // Call the method and verify the result
        assertThrows(V1ServiceException.class, () -> v1ServiceImpl.generateToken());

    }

    @Test
    void testGenerateTokenGetsTokenFromCache() {
        // Mock the token generation API response
        String token = "sampleToken";

        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(mockCache);
        when(mockCache.get(anyString())).thenReturn(() -> token);

        // Call the method and verify the result
        String resultToken = v1ServiceImpl.generateToken();
        assertEquals(token, resultToken);

    }

    @Test
    void testGenerateToken_Success() {
        // Mock the token generation API response
        String token = "sampleToken";
        String v1GenerateTokenUrl = "https://qa-runner.cargoes.com/Api/Account/GenerateToken";
        Map<String, Object> responseBody = new HashMap<>();
        responseBody.put("token", token);

        // Create a non-null ResponseEntity with the mocked response body
        ResponseEntity<Map<String, Object>> responseEntity = new ResponseEntity<>(responseBody, HttpStatus.OK);

        // Properly instantiate ParameterizedTypeReference to match the method
        ParameterizedTypeReference<Map<String, Object>> responseType = new ParameterizedTypeReference<>() {
        };

        // Mock RestTemplate exchange method to return the mocked ResponseEntity
        when(restTemplate.exchange(
                eq(v1GenerateTokenUrl), // Ensure URL matches
                eq(HttpMethod.POST),
                any(),
                eq(responseType)) // Ensure the ParameterizedTypeReference matches
        ).thenReturn(responseEntity);

        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(mockCache);
        when(mockCache.get(anyString())).thenReturn(() -> null);

        // Call the method and verify the result
        String resultToken = v1ServiceImpl.generateToken();
        assertEquals(token, resultToken);

        // Verify the token was retrieved and logged
        verify(restTemplate, times(1)).exchange(eq(v1GenerateTokenUrl), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType));
    }

    @Test
    void testSetAuthContext_Success2() {
        // Mock token and user details
        String token = "Bearer sampleToken";
        String username = "testUser";
        UsersDto user = mock(UsersDto.class);
        when(user.getUsername()).thenReturn(username);
        when(user.getPermissions()).thenReturn(Map.of("READ", true, "WRITE", true));

        // Mock static method calls
        try (MockedStatic<RequestAuthContext> requestAuthContextMock = mockStatic(RequestAuthContext.class);
                MockedStatic<TenantContext> tenantContextMock = mockStatic(TenantContext.class);
                MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class);
                MockedStatic<SecurityContextHolder> securityContextHolderMock = mockStatic(SecurityContextHolder.class);
                MockedStatic<PermissionsContext> permissionsContextMock = mockStatic(PermissionsContext.class)) {

            // Mock security context
            SecurityContext contextMock = mock(SecurityContext.class);
            securityContextHolderMock.when(SecurityContextHolder::getContext).thenReturn(contextMock);

            // Call the method under test
            v1ServiceImpl.setAuthContext(token, user);

            // Verify token and context setting
            requestAuthContextMock.verify(() -> RequestAuthContext.setAuthToken(token), times(1));
            tenantContextMock.verify(() -> TenantContext.setCurrentTenant(user.getTenantId()), times(1));
            userContextMock.verify(() -> UserContext.setUser(user), times(1));

            // Verify the security context was set
            ArgumentCaptor<UsernamePasswordAuthenticationToken> captor = ArgumentCaptor.forClass(UsernamePasswordAuthenticationToken.class);
            verify(contextMock).setAuthentication(captor.capture());
            UsernamePasswordAuthenticationToken authToken = captor.getValue();
            assertNotNull(authToken);
            assertEquals(user, authToken.getPrincipal());
        }
    }

    @Test
    void testSetAuthContextWithEmptyPermissions2() {
        // Mock dependencies
        String token = "Bearer sampleToken";
        String username = "user1";
        UsersDto user = mock(UsersDto.class);
        Map<String, Boolean> permissionsMap = new HashMap<>(); // Empty permissions

        when(user.getTenantId()).thenReturn(1);
        when(user.getUsername()).thenReturn(username);
        when(user.getPermissions()).thenReturn(permissionsMap);

        // Mock static methods
        try (MockedStatic<RequestAuthContext> requestAuthContextMock = mockStatic(RequestAuthContext.class);
                MockedStatic<TenantContext> tenantContextMock = mockStatic(TenantContext.class);
                MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class);
                MockedStatic<SecurityContextHolder> securityContextHolderMock = mockStatic(SecurityContextHolder.class);
                MockedStatic<PermissionsContext> permissionsContextMock = mockStatic(PermissionsContext.class)) {

            // Mock the SecurityContextHolder behavior
            SecurityContext contextMock = mock(SecurityContext.class);
            securityContextHolderMock.when(SecurityContextHolder::getContext).thenReturn(contextMock);

            // Call the method under test
            v1ServiceImpl.setAuthContext(token, user);

            // Verify token and context setting
            requestAuthContextMock.verify(() -> RequestAuthContext.setAuthToken(token), times(1));
            tenantContextMock.verify(() -> TenantContext.setCurrentTenant(1), times(1));
            userContextMock.verify(() -> UserContext.setUser(user), times(1));

            // Verify the security context was set with empty permissions
            List<String> grantedPermissions = new ArrayList<>(permissionsMap.keySet());
            UsernamePasswordAuthenticationToken authenticationToken =
                    new UsernamePasswordAuthenticationToken(user, null, v1ServiceImpl.getAuthorities(grantedPermissions));
            verify(contextMock).setAuthentication(authenticationToken);

            // Verify permissions were set with an empty list
            permissionsContextMock.verify(() -> PermissionsContext.setPermissions(grantedPermissions), times(1));
        }
    }

    @Test
    void testGetAuthorities() {
        // Prepare mock permissions
        List<String> permissions = List.of("READ", "WRITE");

        // Call the method under test
        Collection<? extends GrantedAuthority> authorities = v1ServiceImpl.getAuthorities(permissions);

        // Verify the authorities are correctly mapped
        assertNotNull(authorities);
        assertEquals(2, authorities.size());
        assertTrue(authorities.contains(new SimpleGrantedAuthority("READ")));
        assertTrue(authorities.contains(new SimpleGrantedAuthority("WRITE")));
    }

    @Test
    void getCompaniesDetails() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.getCompaniesDetails("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getCompaniesDetails2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.getCompaniesDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void getCompaniesDetails3() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.getCompaniesDetails("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void listOrgs() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.listOrgs("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void listOrgsThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listOrgs("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void listOrgsThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.listOrgs("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void listBranchesByDefaultOrgAndAddress() {
        var mockResponse = V1DataResponse.builder().build();
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(mockResponse));
        // Act
        var responseEntity = v1ServiceImpl.listBranchesByDefaultOrgAndAddress("Request");

        // Assert
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void listBranchesByDefaultOrgAndAddressThrowsError() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new HttpClientErrorException(HttpStatus.UNAUTHORIZED));
        // Act
        var throwable = assertThrows(Throwable.class, () -> v1ServiceImpl.listBranchesByDefaultOrgAndAddress("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void listBranchesByDefaultOrgAndAddressThrowsError2() {
        // Arrange
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException());
        // Act
        var throwable = assertThrows(V1ServiceException.class, () -> v1ServiceImpl.listBranchesByDefaultOrgAndAddress("Request"));

        // Assert
        assertNotNull(throwable);
    }

    @Test
    void testGetUsersWithGivenPermissions_Success() {
        UserWithPermissionRequestV1 mockRequest = new UserWithPermissionRequestV1();
        List<UsersDto> users = List.of(new UsersDto(), new UsersDto());
        V1DataResponse mockResponse = V1DataResponse.builder().entities(users).build();

        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(new ResponseEntity<>(mockResponse, HttpStatus.OK));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(new ArrayList<>());

        List<UsersDto> result = v1ServiceImpl.getUsersWithGivenPermissions(mockRequest);

        assertNotNull(result);
    }

    @Test
    void testGetUsersWithGivenPermissions_ClientError() {
        UserWithPermissionRequestV1 mockRequest = new UserWithPermissionRequestV1();
        HttpClientErrorException exception = HttpClientErrorException.create(
                HttpStatus.BAD_REQUEST,
                "Bad Request",
                null,
                "{\"error\": {\"message\": \"Client Error\"}}".getBytes(),
                null
        );

        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(exception);

        when(jsonHelper.readFromJson(anyString(), eq(V1ErrorResponse.class)))
                .thenReturn(new V1ErrorResponse(new V1ErrorResponse.V1Error("Client Error")));

        V1ServiceException thrown = assertThrows(
                V1ServiceException.class,
                () -> v1ServiceImpl.getUsersWithGivenPermissions(mockRequest)
        );

        assertEquals("Client Error", thrown.getMessage());
    }

    @Test
    void testGetUsersWithGivenPermissions_GenericException() {
        UserWithPermissionRequestV1 mockRequest = new UserWithPermissionRequestV1();

        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenThrow(new RuntimeException("Unexpected error"));

        V1ServiceException thrown = assertThrows(
                V1ServiceException.class,
                () -> v1ServiceImpl.getUsersWithGivenPermissions(mockRequest)
        );

        assertEquals("Unexpected error", thrown.getMessage());
    }
}
