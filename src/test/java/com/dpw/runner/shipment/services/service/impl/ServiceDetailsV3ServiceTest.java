package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkServiceDetailsResponse;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.ServiceDetailsV3Util;
import com.dpw.runner.shipment.services.utils.v3.ServiceDetailsValidationV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@SuppressWarnings("java:S6068")
@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ServiceDetailsV3ServiceTest {

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ServiceDetailsValidationV3Util serviceDetailsValidationV3Util;

    @Mock
    private IServiceDetailsDao serviceDetailsDao;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private ExecutorService executorServiceMasterData;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private ServiceDetailsV3Util serviceDetailsV3Util;

    @InjectMocks
    private ServiceDetailsV3Service serviceDetailsV3Service;

    private static final String TEST_GUID = "550e8400-e29b-41d4-a716-446655440000";

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);

        serviceDetailsV3Service.executorServiceMasterData = Executors.newFixedThreadPool(2);
    }

    @AfterEach
    void tearDown() {
        serviceDetailsV3Service.executorServiceMasterData.shutdown();
    }

    @Test
    void testCreate_AuditLogException_ShouldNotFailOperation() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        ServiceDetailsRequest request = createServiceDetailsRequest();
        ServiceDetails serviceDetails = createServiceDetails();
        ServiceDetailsResponse response = createServiceDetailsResponse();

        when(jsonHelper.convertValue(request, ServiceDetails.class)).thenReturn(serviceDetails);
        when(serviceDetailsDao.save(any(ServiceDetails.class))).thenReturn(serviceDetails);
        when(jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class)).thenReturn(response);
        doThrow(new RuntimeException("Audit log failed")).when(auditLogService).addAuditLog(any());

        // Act
        ServiceDetailsResponse result = serviceDetailsV3Service.create(request, Constants.SHIPMENT);

        // Assert
        assertNotNull(result);
        assertEquals(response, result);
        verify(serviceDetailsDao).save(any(ServiceDetails.class));
    }

    @Test
    void testRetrieveById_ById_Success() {
        // Arrange
        Long id = 1L;
        ServiceDetails serviceDetails = createServiceDetails();
        ServiceDetailsResponse response = createServiceDetailsResponse();

        when(serviceDetailsDao.findById(id)).thenReturn(Optional.of(serviceDetails));
        when(jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class)).thenReturn(response);

        // Act
        ServiceDetailsResponse result = serviceDetailsV3Service.retrieveById(id, null, null);

        // Assert
        assertNotNull(result);
        assertEquals(response, result);
        verify(serviceDetailsDao).findById(id);
    }

    @Test
    void testRetrieveById_ByGuid_Success() {
        // Arrange
        UUID guid = UUID.fromString(TEST_GUID);
        ServiceDetails serviceDetails = createServiceDetails();
        ServiceDetailsResponse response = createServiceDetailsResponse();

        when(serviceDetailsDao.findByGuid(guid)).thenReturn(Optional.of(serviceDetails));
        when(jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class)).thenReturn(response);

        // Act
        ServiceDetailsResponse result = serviceDetailsV3Service.retrieveById(null, TEST_GUID, null);

        // Assert
        assertNotNull(result);
        assertEquals(response, result);
        verify(serviceDetailsDao).findByGuid(guid);
        verify(serviceDetailsDao, never()).findById(any());
    }

    @Test
    void testRetrieveById_ValidationException() {
        when(serviceDetailsDao.findById(1L)).thenReturn(Optional.empty());

        assertThrows(ValidationException.class, () -> serviceDetailsV3Service.retrieveById(1L, null, null));
    }

    @Test
    void testRetrieveById_ValidationException2() {
        when(serviceDetailsDao.findById(1L)).thenThrow(new RuntimeException());

        assertThrows(ValidationException.class, () -> serviceDetailsV3Service.retrieveById(1L, null, null));
    }

    @Test
    void testRetrieveById_BothIdAndGuidNull_ThrowsException() {
        // Act & Assert
        assertThrows(ValidationException.class,
                () -> serviceDetailsV3Service.retrieveById(null, null, null));
    }

    @Test
    void testRetrieveById_NetworkTransfer_ById() {
        // Arrange
        Long id = 1L;
        ServiceDetails serviceDetails = createServiceDetails();
        ServiceDetailsResponse response = createServiceDetailsResponse();

        when(serviceDetailsDao.findByIdWithQuery(id)).thenReturn(Optional.of(serviceDetails));
        when(jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class)).thenReturn(response);

        // Act
        ServiceDetailsResponse result = serviceDetailsV3Service.retrieveById(id, null, Constants.NETWORK_TRANSFER);

        // Assert
        assertNotNull(result);
        assertEquals(response, result);
        verify(serviceDetailsDao).findByIdWithQuery(id);
    }

    @Test
    void testRetrieveById_NetworkTransfer_ByGuid() {
        // Arrange
        UUID guid = UUID.fromString(TEST_GUID);
        ServiceDetails serviceDetails = createServiceDetails();
        ServiceDetailsResponse response = createServiceDetailsResponse();

        when(serviceDetailsDao.findByGuidWithQuery(guid)).thenReturn(Optional.of(serviceDetails));
        when(jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class)).thenReturn(response);

        // Act
        ServiceDetailsResponse result = serviceDetailsV3Service.retrieveById(null, TEST_GUID, Constants.NETWORK_TRANSFER);

        // Assert
        assertNotNull(result);
        assertEquals(response, result);
        verify(serviceDetailsDao).findByGuidWithQuery(guid);
    }

    @Test
    void testUpdateBulk_EmptyUpdateAndCreateLists() {
        // Arrange
        List<ServiceDetailsRequest> requestList = List.of(createServiceDetailsRequest());

        when(jsonHelper.convertValueToList(anyList(), eq(ServiceDetails.class))).thenReturn(new ArrayList<>());

        assertThrows(IndexOutOfBoundsException.class, () -> serviceDetailsV3Service.updateBulk(requestList, Constants.SHIPMENT));
    }

    @Test
    void testUpdateBulk_MixedCreateAndUpdate() throws RunnerException {
        // Arrange
        ServiceDetailsRequest updateRequest = createServiceDetailsRequest();
        updateRequest.setId(1L);
        ServiceDetailsRequest createRequest = createServiceDetailsRequest();

        List<ServiceDetailsRequest> requestList = Arrays.asList(updateRequest, createRequest);
        ServiceDetails existingDetail1 = createServiceDetails();
        existingDetail1.setId(1L);

        ServiceDetailsResponse serviceDetailsResponse = createServiceDetailsResponse();
        serviceDetailsResponse.setId(2L);

        when(serviceDetailsDao.findByIdIn(anyList())).thenReturn(Arrays.asList(existingDetail1));
        when(jsonHelper.convertValueToList(eq(List.of(existingDetail1)), eq(ServiceDetails.class))).thenReturn(Arrays.asList(existingDetail1));
        when(jsonHelper.convertValueToList(eq(List.of(updateRequest)), eq(ServiceDetails.class))).thenReturn(Arrays.asList(existingDetail1));
        when(jsonHelper.convertValueToList(eq(List.of(createRequest)), eq(ServiceDetails.class))).thenReturn(Arrays.asList(createServiceDetails()));
        when(serviceDetailsDao.saveAll(eq(List.of(createServiceDetails())))).thenReturn(Arrays.asList(createServiceDetails()));
        when(jsonHelper.convertValueToList(anyList(), eq(ServiceDetailsResponse.class)))
                .thenReturn(Arrays.asList(createServiceDetailsResponse(), serviceDetailsResponse));

        // Act
        BulkServiceDetailsResponse result = serviceDetailsV3Service.updateBulk(requestList, Constants.SHIPMENT);

        // Assert
        assertNotNull(result);
        assertEquals(2, result.getServiceDetailsResponseList().size());
        assertTrue(result.getMessage().contains("Bulk edit success!"));
    }

    @Test
    void testDeleteBulk_NoServiceDetailsFound_ThrowsException() {
        // Arrange
        List<ServiceDetailsRequest> requestList = Arrays.asList(createServiceDetailsRequest());
        requestList.get(0).setId(999L);

        when(serviceDetailsDao.findByIdIn(anyList())).thenReturn(new ArrayList<>());

        // Act & Assert
        assertThrows(DataRetrievalFailureException.class,
                () -> serviceDetailsV3Service.deleteBulk(requestList, Constants.SHIPMENT));
    }

    @Test
    void testDeleteBulkSuccess() throws RunnerException {
        // Arrange
        List<ServiceDetailsRequest> requestList = Arrays.asList(createServiceDetailsRequest());
        requestList.get(0).setId(999L);

        when(serviceDetailsDao.findByIdIn(anyList())).thenReturn(List.of(createServiceDetails()));

        doNothing().when(serviceDetailsValidationV3Util).validateUpdateBulkRequest(anyList(), anyList());
        doNothing().when(serviceDetailsDao).deleteByIdIn(anyList());

        var result = serviceDetailsV3Service.deleteBulk(requestList, Constants.SHIPMENT);

        assertNotNull(result);
        assertTrue(result.getMessage().contains("Service Details deleted successfully!"));
    }

    @Test
    void testDeleteBulkSuccess2() throws RunnerException {
        // Arrange
        List<ServiceDetailsRequest> requestList = Arrays.asList(createServiceDetailsRequest(), createServiceDetailsRequest());
        requestList.get(0).setId(3L);
        requestList.get(1).setId(1000L);

        ServiceDetails serviceDetails = createServiceDetails();
        serviceDetails.setId(1000L);

        when(serviceDetailsDao.findByIdIn(anyList())).thenReturn(List.of(createServiceDetails(), serviceDetails));

        doNothing().when(serviceDetailsValidationV3Util).validateUpdateBulkRequest(anyList(), anyList());
        doNothing().when(serviceDetailsDao).deleteByIdIn(anyList());

        var result = serviceDetailsV3Service.deleteBulk(requestList, Constants.SHIPMENT);

        assertNotNull(result);
        assertTrue(result.getMessage().contains("All selected service details deleted successfully!"));
    }

    @Test
    void testDelete_WithNullId() {
        assertThrows(IllegalArgumentException.class, () -> serviceDetailsV3Service.delete(null, Constants.SHIPMENT));
    }

    @Test
    void testDelete_ServiceDetailsNotFound() {
        when(serviceDetailsDao.findById(1L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> serviceDetailsV3Service.delete(1L, Constants.SHIPMENT));
    }

    @Test
    void testDelete_WithServiceCount() throws RunnerException {
        // Arrange
        ServiceDetails serviceDetails = createServiceDetails();

        when(serviceDetailsDao.findById(1L)).thenReturn(Optional.of(serviceDetails));

        // Act
        String result = serviceDetailsV3Service.delete(1L, Constants.SHIPMENT);

        // Assert
        assertTrue(result.contains("Service Details CLN - 1 deleted successfully!"));
    }

    @Test
    void testDelete_WithNullServiceCount() throws RunnerException {
        // Arrange
        ServiceDetails serviceDetails = createServiceDetails();
        serviceDetails.setServiceCount(null);

        when(serviceDetailsDao.findById(1L)).thenReturn(Optional.of(serviceDetails));

        // Act
        String result = serviceDetailsV3Service.delete(1L, Constants.SHIPMENT);

        // Assert
        assertTrue(result.contains("Service Details CLN deleted successfully!"));
        assertFalse(result.contains(" - "));
    }

    @Test
    void testList_NetworkTransfer() {
        // Arrange
        ListCommonRequest request = createListCommonRequest();
        ServiceDetails serviceDetails = createServiceDetails();
        Page<ServiceDetails> page = new PageImpl<>(Arrays.asList(serviceDetails));

        when(serviceDetailsDao.findAllWithoutTenantFilter(any(), any()))
                .thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any()))
                .thenReturn(createServiceDetailsResponse());

        // Act
        ServiceDetailsListResponse result = serviceDetailsV3Service.list(request, false, Constants.NETWORK_TRANSFER);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.getTotalCount());
        verify(serviceDetailsDao).findAllWithoutTenantFilter(any(), any());
        verify(serviceDetailsDao, never()).findAll(any(), any());
    }

    @Test
    void testList_NullRequest() {
        assertThrows(ValidationException.class, () -> serviceDetailsV3Service.list(null, false, null));
    }

    @Test
    void testList_NullPage() {
        // Arrange
        ListCommonRequest request = createListCommonRequest();

        when(serviceDetailsDao.findAll(any(), any())).thenReturn(null);

        // Act
        ServiceDetailsListResponse result = serviceDetailsV3Service.list(request, false, null);

        // Assert
        assertNotNull(result);
    }

    @Test
    void testFetchShipmentServices_WithExistingFilterCriteria() {
        // Arrange
        ListCommonRequest request = createListCommonRequest();
        request.setEntityId("123");
        FilterCriteria filterCriteria = new FilterCriteria();
        filterCriteria.setInnerFilter(new ArrayList<>());
        request.setFilterCriteria(Arrays.asList(filterCriteria));

        Page<ServiceDetails> page = new PageImpl<>(Arrays.asList(createServiceDetails()));

        when(serviceDetailsDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any()))
                .thenReturn(createServiceDetailsResponse());

        // Act
        ServiceDetailsListResponse result = serviceDetailsV3Service.fetchShipmentServices(request, null);

        // Assert
        assertNotNull(result);
    }

    @Test
    void testGetAllMasterData_ExceptionHandling() {
        // Arrange
        when(serviceDetailsDao.findById(1L)).thenThrow(new RuntimeException("Database error"));

        // Act
        Map<String, Object> result = serviceDetailsV3Service.getAllMasterData(1L, null);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetAllMasterData_ExceptionHandling2() {
        // Arrange
        when(serviceDetailsDao.findById(1L)).thenReturn(Optional.empty());

        Map<String, Object> result = serviceDetailsV3Service.getAllMasterData(1L, null);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetAllMasterData_NetworkTransfer() {
        // Arrange
        ServiceDetails serviceDetails = createServiceDetails();

        when(serviceDetailsDao.findByIdWithQuery(1L)).thenReturn(Optional.of(serviceDetails));
        when(jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class))
                .thenReturn(createServiceDetailsResponse());

        // Act
        Map<String, Object> result = serviceDetailsV3Service.getAllMasterData(1L, Constants.NETWORK_TRANSFER);

        // Assert
        assertNotNull(result);
        verify(serviceDetailsDao).findByIdWithQuery(1L);
        verify(serviceDetailsDao, never()).findById(any());
    }

    @Test
    void testFetchAllMasterDataByKey_shouldRunAllAsyncCallsAndReturnMap() {
        // Mock withMdc to return the same Runnable
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // Mock all master data helper methods to just modify the map for visibility
        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("master", "ok");
            return null;
        }).when(serviceDetailsV3Util).addAllMasterDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("unlocation", "ok");
            return null;
        }).when(serviceDetailsV3Util).addAllUnlocationDataInSingleCall(any(), any());

        // Call method under test
        Map<String, Object> responseMap = serviceDetailsV3Service.fetchAllMasterDataByKey(createServiceDetailsResponse());

        // Validate map contains all expected keys
        assertEquals(2, responseMap.size());
        assertEquals("ok", responseMap.get("master"));
        assertEquals("ok", responseMap.get("unlocation"));

        // Optional: verify method calls
        verify(serviceDetailsV3Util).addAllMasterDataInSingleCall(any(), any());
        verify(serviceDetailsV3Util).addAllUnlocationDataInSingleCall(any(), any());
    }

    @Test
    void testFetchShipmentServices_InvalidEntityId() {
        // Arrange
        ListCommonRequest request = createListCommonRequest();
        request.setEntityId("-1");

        // Act & Assert
        assertThrows(ValidationException.class,
                () -> serviceDetailsV3Service.fetchShipmentServices(request, null));
    }

    // Helper methods
    private ServiceDetailsRequest createServiceDetailsRequest() {
        ServiceDetailsRequest request = new ServiceDetailsRequest();
        request.setServiceType("CLN");
        request.setServiceCount(1L);
        return request;
    }

    private ServiceDetails createServiceDetails() {
        ServiceDetails details = new ServiceDetails();
        details.setId(3L);
        details.setGuid(UUID.fromString(TEST_GUID));
        details.setServiceType("CLN");
        details.setServiceCount(1L);
        details.setShipmentId(100L);
        return details;
    }

    private ServiceDetailsResponse createServiceDetailsResponse() {
        ServiceDetailsResponse response = new ServiceDetailsResponse();
        response.setId(1L);
        response.setGuid(UUID.fromString(TEST_GUID));
        response.setServiceType("CLN");
        response.setServiceCount(1L);
        return response;
    }

    private ListCommonRequest createListCommonRequest() {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(1);
        request.setPageSize(10);
        request.setFilterCriteria(new ArrayList<>());
        return request;
    }
}