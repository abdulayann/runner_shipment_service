package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryV3Response;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.PackingListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignPackageContainerRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import com.dpw.runner.shipment.services.projection.PackingAssignmentProjection;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.PackingV3Util;
import com.dpw.runner.shipment.services.utils.v3.PackingValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.http.auth.AuthenticationException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
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

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class})
@Execution(CONCURRENT)
class PackingV3ServiceTest extends CommonMocks {

    @InjectMocks
    private PackingV3Service packingV3Service;

    @Mock
    private IPackingDao packingDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IContainerV3Service containerV3Service;
    @Mock
    private ICustomerBookingV3Service customerBookingV3Service;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private PackingValidationV3Util packingValidationV3Util;
    @Mock
    private IShipmentServiceV3 shipmentService;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private IConsolidationV3Service consolidationService;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private PackingV3Util packingV3Util;
    @Mock
    private DependentServiceHelper dependentServiceHelper;
    @Mock
    private HttpServletResponse httpServletResponse;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private ShipmentValidationV3Util shipmentValidationV3Util;

    private Packing packing;
    private PackingV3Request request;
    private PackingResponse response;

    private static JsonTestUtility jsonTestUtility;

    private static ShipmentDetails testShipment;

    private static ConsolidationDetails testconsol;

    @BeforeAll
    static void init() {
        try {
            jsonTestUtility = new JsonTestUtility();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setup() {
        request = new PackingV3Request();
        request.setId(1L);
        request.setShipmentId(100L);
        request.setBookingId(200L);
        request.setConsolidationId(300L);

        packing = new Packing();
        packing.setId(1L);
        packing.setShipmentId(100L);
        packing.setPacks("5");
        packing.setPacksType("BAG");
        packing.setWeight(BigDecimal.valueOf(5L));
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(10L));
        packing.setVolumeUnit("M3");

        response = new PackingResponse();
        response.setId(1L);
        response.setShipmentId(100L);

        testShipment = jsonTestUtility.getTestShipment();
        testShipment.setShipmentType("LCL");
        testShipment.setId(1L);

        testconsol = jsonTestUtility.getTestConsolidation();
        testconsol.setShipmentsList(Collections.singleton(testShipment));

        packingV3Service.executorServiceMasterData = Executors.newFixedThreadPool(2);

        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").multipleShipmentEnabled(true).enableLclConsolidation(true).build());
    }

    @AfterEach
    void tearDown() {
        packingV3Service.executorServiceMasterData.shutdown();
    }

    @Test
    void testCreatePacking_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.valueOf(150));
        volumeWeightChargeable.setVolumeWeight(BigDecimal.valueOf(100));
        testShipment.setDirection("EXP");

        when(jsonHelper.convertValue(request, Packing.class)).thenReturn(packing);
        when(shipmentService.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(packingDao.save(packing)).thenReturn(packing);
        when(jsonHelper.convertValue(packing, PackingResponse.class)).thenReturn(response);
        when(packingDao.findByShipmentId(anyLong())).thenReturn(List.of(packing));
        when(shipmentService.findById(anyLong())).thenReturn(Optional.of(testShipment));
        doNothing().when(auditLogService).addAuditLog(any());
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        doNothing().when(shipmentService).updateCargoDetailsInShipment(any(), any());
        mockShipmentSettings();

        PackingResponse actual = packingV3Service.create(request, "SHIPMENT");

        assertEquals(response.getId(), actual.getId());
        verify(packingDao).save(packing);
    }

    @Test
    void testCreatePacking_exception() {
        request.setContainerId(1L);

        assertThrows(ValidationException.class, () -> packingV3Service.create(request, "SHIPMENT"));
    }

    @Test
    void testUpdatePacking_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.valueOf(150));
        volumeWeightChargeable.setVolumeWeight(BigDecimal.valueOf(100));
        testShipment.setTransportMode("AIR");

        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));
        when(packingValidationV3Util.validateModule(any(), anyString())).thenReturn(testShipment);
        when(jsonHelper.convertValue(any(), eq(Packing.class))).thenReturn(packing);
        when(packingDao.save(packing)).thenReturn(packing);
        when(packingDao.findByShipmentId(anyLong())).thenReturn(List.of(packing));
        when(jsonHelper.convertValue(any(), eq(PackingResponse.class))).thenReturn(response);
        doNothing().when(auditLogService).addAuditLog(any());
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        doNothing().when(shipmentService).updateCargoDetailsInShipment(any(), any());

        PackingResponse result = packingV3Service.update(request, "SHIPMENT");

        assertEquals(1L, result.getId());
    }

    @Test
    void testUpdatePacking_exception() {
        request.setContainerId(1L);
        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));
        when(packingValidationV3Util.validateModule(any(), anyString())).thenReturn(testShipment);

        assertThrows(ValidationException.class, () -> packingV3Service.update(request, "SHIPMENT"));
    }

    @Test
    void testDeletePacking_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.valueOf(150));
        volumeWeightChargeable.setVolumeWeight(BigDecimal.valueOf(100));
        testShipment.setTransportMode("AIR");
        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));
        when(shipmentService.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(packingV3Util.getConsolidationId(anyLong())).thenReturn(1L);
        doNothing().when(auditLogService).addAuditLog(any());
        when(packingDao.findByShipmentId(anyLong())).thenReturn(List.of(packing));
        when(shipmentService.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        doNothing().when(shipmentService).updateCargoDetailsInShipment(any(), any());

        String result = packingV3Service.delete(1L, "SHIPMENT");

        assertTrue(result.contains("deleted successfully"));
        verify(packingDao).delete(packing);
    }

    @Test
    void testDeletePacking_success2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        packing.setPacksType(null);
        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));
        doNothing().when(auditLogService).addAuditLog(any());

        String result = packingV3Service.delete(1L, "BOOKING");

        assertTrue(result.contains("deleted successfully"));
        verify(packingDao).delete(packing);
    }

    @Test
    void testDeletePacking_ValidationException() {
        packing.setContainerId(1L);
        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));

        assertThrows(ValidationException.class, () -> packingV3Service.delete(1L, "SHIPMENT"));
    }

    @Test
    void testUpdateBulk_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PackingV3Request newRequest = new PackingV3Request();
        List<PackingV3Request> requestList = List.of(request, newRequest);
        testShipment.setDirection(null);

        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));
        when(packingValidationV3Util.validateModule(any(), anyString())).thenReturn(testShipment);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(List.of(packing));
        when(packingDao.saveAll(anyList())).thenReturn(List.of(packing));
        when(jsonHelper.convertValueToList(anyList(), eq(PackingResponse.class))).thenReturn(List.of(response));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkPackingResponse result = packingV3Service.updateBulk(requestList, "SHIPMENT");

        assertNotNull(result.getPackingResponseList());
        assertEquals(1, result.getPackingResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void testUpdateBulk_success2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PackingV3Request newRequest = new PackingV3Request();
        List<PackingV3Request> requestList = List.of(newRequest, newRequest);

        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(List.of(packing));
        when(packingDao.saveAll(anyList())).thenReturn(List.of(packing));
        when(jsonHelper.convertValueToList(anyList(), eq(PackingResponse.class))).thenReturn(List.of(response, response));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkPackingResponse result = packingV3Service.updateBulk(requestList, "CONSOLIDATION");

        assertNotNull(result.getPackingResponseList());
        assertEquals(2, result.getPackingResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void testUpdateBulk_success3() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PackingV3Request newRequest = new PackingV3Request();
        List<PackingV3Request> requestList = List.of(request, newRequest);
        testShipment.setTransportMode("AIR");

        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));
        when(packingValidationV3Util.validateModule(any(), anyString())).thenReturn(testShipment);
        when(packingV3Util.updateConsolidationIdInPackings(any(), anyList())).thenReturn(1L);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(List.of(packing));
        when(packingDao.saveAll(anyList())).thenReturn(List.of(packing));
        when(jsonHelper.convertValueToList(anyList(), eq(PackingResponse.class))).thenReturn(List.of(response));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkPackingResponse result = packingV3Service.updateBulk(requestList, "SHIPMENT");

        assertNotNull(result.getPackingResponseList());
        assertEquals(1, result.getPackingResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void testDeleteBulk_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        testShipment.setDirection("EXP");
        when(shipmentService.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));
        doNothing().when(auditLogService).addAuditLog(any());
        mockShipmentSettings();

        BulkPackingResponse result = packingV3Service.deleteBulk(List.of(request), "SHIPMENT");

        assertTrue(result.getMessage().contains("deleted successfully"));
        verify(packingDao).deleteByIdIn(anyList());
        verify(auditLogService).addAuditLog(any());
    }

    @Test
    void testDeleteBulk_success2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Packing packing1 = new Packing();
        packing1.setId(2L);
        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing, packing1));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkPackingResponse result = packingV3Service.deleteBulk(List.of(request), "BOOKING");

        assertTrue(result.getMessage().contains("deleted successfully"));
        verify(packingDao).deleteByIdIn(anyList());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void testDeleteBulk_success3() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        testShipment.setDirection("EXP");
        testShipment.setShipmentType(null);
        when(shipmentService.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkPackingResponse result = packingV3Service.deleteBulk(List.of(request), "SHIPMENT");

        assertTrue(result.getMessage().contains("deleted successfully"));
        verify(packingDao).deleteByIdIn(anyList());
        verify(auditLogService).addAuditLog(any());
    }

    @SuppressWarnings("java:S5778")
    @Test
    void testDeleteBulk_ValidationException() {
        packing.setContainerId(1L);
        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));

        assertThrows(ValidationException.class, () -> packingV3Service.deleteBulk(List.of(request), "SHIPMENT"));
    }

    @Test
    void testUpdatePacking_notFound() {
        when(packingDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () ->
                packingV3Service.update(request, "SHIPMENT")
        );
    }

    @Test
    void testDeletePacking_idIsNull() {
        assertThrows(IllegalArgumentException.class, () ->
                packingV3Service.delete(null, "SHIPMENT")
        );
    }

    @Test
    void testDeletePacking_notFound() {
        when(packingDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () ->
                packingV3Service.delete(1L, "SHIPMENT")
        );
    }

    @Test
    void testDeleteBulkPacking_notFound() {
        PackingV3Request newRequest = new PackingV3Request();
        List<PackingV3Request> requestList = List.of(request, newRequest);
        when(packingDao.findByIdIn(any())).thenReturn(new ArrayList<>());

        assertThrows(DataRetrievalFailureException.class, () ->
                packingV3Service.deleteBulk(requestList, "SHIPMENT")
        );
    }

    @Test
    void testRetrieveById_success() {
        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));
        when(jsonHelper.convertValue(packing, PackingResponse.class)).thenReturn(response);

        PackingResponse actualResponse = packingV3Service.retrieveById(1L, null, null);

        assertNotNull(actualResponse);
        assertEquals(1L, actualResponse.getId());
    }

    @Test
    void testRetrieveByIdForNTE_success() {
        when(packingDao.findByIdWithQuery(1L)).thenReturn(Optional.of(packing));
        when(jsonHelper.convertValue(packing, PackingResponse.class)).thenReturn(response);

        PackingResponse actualResponse = packingV3Service.retrieveById(1L, null, Constants.NETWORK_TRANSFER);

        assertNotNull(actualResponse);
        assertEquals(1L, actualResponse.getId());
    }

    @Test
    void testRetrieveById_guidSuccess() {
        UUID guid = UUID.randomUUID();
        when(packingDao.findByGuid(guid)).thenReturn(Optional.of(packing));
        when(jsonHelper.convertValue(packing, PackingResponse.class)).thenReturn(response);

        PackingResponse actualResponse = packingV3Service.retrieveById(null, guid.toString(), null);

        assertNotNull(actualResponse);
    }

    @Test
    void testRetrieveByIdForNTE_guidSuccess() {
        UUID guid = UUID.randomUUID();
        when(packingDao.findByGuidWithQuery(guid)).thenReturn(Optional.of(packing));
        when(jsonHelper.convertValue(packing, PackingResponse.class)).thenReturn(response);

        PackingResponse actualResponse = packingV3Service.retrieveById(null, guid.toString(), Constants.NETWORK_TRANSFER);

        assertNotNull(actualResponse);
    }

    @Test
    void testRetrieveById_invalidRequest() {
        assertThrows(ValidationException.class, () -> packingV3Service.retrieveById(null, null, null));
    }

    @Test
    void testRetrieveById_notFound() {
        when(packingDao.findById(1L)).thenReturn(Optional.empty());

        assertThrows(ValidationException.class, () -> packingV3Service.retrieveById(1L, null, null));
    }

    @Test
    void testList_success() {
        CommonRequestModel model = CommonRequestModel.builder().build();
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        model.setData(listCommonRequest);

        Page<Packing> page = new PageImpl<>(List.of(packing));
        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);

        PackingListResponse actualResponse = packingV3Service.list(listCommonRequest, true, null);

        assertEquals(1, actualResponse.getPackings().size());
        assertEquals(1, actualResponse.getTotalCount());
    }

    @Test
    void testListForNTE_success() {
        CommonRequestModel model = CommonRequestModel.builder().build();
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        model.setData(listCommonRequest);

        Page<Packing> page = new PageImpl<>(List.of(packing));
        when(packingDao.findAllWithoutTenantFilter(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);

        PackingListResponse actualResponse = packingV3Service.list(listCommonRequest, false, Constants.NETWORK_TRANSFER);

        assertEquals(1, actualResponse.getPackings().size());
        assertEquals(1, actualResponse.getTotalCount());
    }

    @Test
    void testFetchShipmentPackages_withAssignedCounts() {
        CommonRequestModel model = CommonRequestModel.builder().build();
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setEntityId("123");
        model.setData(listCommonRequest);

        PackingAssignmentProjection projection = mock(PackingAssignmentProjection.class);
        when(projection.getAssignedCount()).thenReturn(2L);
        when(projection.getUnassignedCount()).thenReturn(3L);

        Page<Packing> page = new PageImpl<>(List.of(packing));
        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);
        when(packingDao.getPackingAssignmentCountByShipmentAndTenant(anyLong(), anyInt())).thenReturn(projection);

        PackingListResponse actualResponse = packingV3Service.fetchShipmentPackages(listCommonRequest, null);

        assertEquals(2L, actualResponse.getAssignedPackageCount());
        assertEquals(3L, actualResponse.getUnassignedPackageCount());
    }

    @Test
    void testFetchShipmentPackages2() {
        CommonRequestModel model = CommonRequestModel.builder().build();
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setEntityId("123");
        model.setData(listCommonRequest);

        response.setShipmentId(null);
        PackingAssignmentProjection projectionMock = mock(PackingAssignmentProjection.class);

        Page<Packing> page = new PageImpl<>(List.of(packing));
        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);
        when(packingDao.getPackingAssignmentCountByShipmentAndTenant(anyLong(), anyInt())).thenReturn(projectionMock);

        PackingListResponse actualResponse = packingV3Service.fetchShipmentPackages(listCommonRequest, null);

        assertNotNull(actualResponse);
    }

    @Test
    void testGetAllMasterData_success() {
        when(packingDao.findById(1L)).thenReturn(Optional.of(packing));

        Map<String, Object> result = packingV3Service.getAllMasterData(1L, null);

        assertNotNull(result);
    }

    @Test
    void testGetAllMasterDataForNTE_success() {
        when(packingDao.findByIdWithQuery(1L)).thenReturn(Optional.of(packing));

        Map<String, Object> result = packingV3Service.getAllMasterData(1L, Constants.NETWORK_TRANSFER);

        assertNotNull(result);
    }

    @Test
    void testGetAllMasterData_dataNotFound() {
        when(packingDao.findById(1L)).thenReturn(Optional.empty());

        Map<String, Object> result = packingV3Service.getAllMasterData(1L, null);

        assertTrue(result.isEmpty());
    }

    @Test
    void testList_requestNull_logsError() {
        assertThrows(ValidationException.class, () -> packingV3Service.list(null, false, null));
    }

    @Test
    void testGetAllMasterData_packingNotFound_returnsEmptyMap() {
        when(packingDao.findById(99L)).thenReturn(Optional.empty());

        Map<String, Object> result = packingV3Service.getAllMasterData(99L, null);

        assertTrue(result.isEmpty());
    }

    @Test
    void testGetAllMasterData_internalException_returnsEmptyMap() {
        when(packingDao.findById(1L)).thenThrow(new RuntimeException("Database failure"));

        Map<String, Object> result = packingV3Service.getAllMasterData(99L, null);

        assertTrue(result.isEmpty());
    }

    @Test
    void testDownloadPacking() throws RunnerException {
        BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
        doNothing().when(packingV3Util).downloadPacking(any(), any());

        packingV3Service.downloadPacking(httpServletResponse, bulkDownloadRequest);

        verify(packingV3Util, times(1)).downloadPacking(any(), any());
    }

    @Test
    void testFetchPacksAttachedToContainers_returnsPackingResponseList() {
        List<Long> containerIds = List.of(1L, 2L);
        List<Packing> mockPackingList = List.of(new Packing(), new Packing());

        when(packingDao.findByContainerIdIn(containerIds)).thenReturn(mockPackingList);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);

        List<PackingResponse> result = packingV3Service.fetchPacksAttachedToContainers(containerIds);

        assertEquals(2, result.size());
        verify(packingDao).findByContainerIdIn(containerIds);
    }

    @Test
    void testRemoveContainersFromPacking_shouldInvokeDao() {
        List<Long> containerIds = List.of(10L, 20L);

        packingV3Service.removeContainersFromPacking(containerIds);

        verify(packingDao).removeContainersFromPacking(containerIds);
    }

    @Test
    void testFilterContainerIdsAttachedToPacking_shouldReturnEmptyList() {
        List<Long> containerIds = List.of(100L, 200L);

        List<Long> result = packingV3Service.filterContainerIdsAttachedToPacking(containerIds);

        assertTrue(result.isEmpty());
    }

    @Test
    void testProcessPacksAfterShipmentAttachment_airMode_shouldSavePackings() {
        Long consolidationId = 99L;

        Packing packing1 = new Packing();
        Packing packing2 = new Packing();

        List<Packing> packingList = List.of(packing1, packing2);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(packingList);

        packingV3Service.processPacksAfterShipmentAttachment(consolidationId, shipmentDetails);

        assertEquals(consolidationId, packing1.getConsolidationId());
        assertEquals(consolidationId, packing2.getConsolidationId());

        verify(packingDao).saveAll(packingList);
    }

    @Test
    void testProcessPacksAfterShipmentAttachment_nonAirMode_shouldDoNothing() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("SEA");
        shipmentDetails.setPackingList(List.of(new Packing()));

        packingV3Service.processPacksAfterShipmentAttachment(1L, shipmentDetails);

        verify(packingDao, never()).saveAll(any());
    }
    @Test
    void testProcessPacksAfterShipmentAttachment_AirMode() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");
        shipmentDetails.setPackingList(List.of(new Packing()));

        packingV3Service.processPacksAfterShipmentAttachment(1L, shipmentDetails);

        verify(packingDao, times(1)).saveAll(any());
    }
    @Test
    void testProcessPacksAfterShipmentAttachment_nullPackingList_shouldDoNothing() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(null);

        packingV3Service.processPacksAfterShipmentAttachment(1L, shipmentDetails);

        verify(packingDao, never()).saveAll(any());
    }

    @Test
    void testFetchAllMasterDataByKey_shouldRunAllAsyncCallsAndReturnMap() {
        PackingResponse packingResponse = new PackingResponse();

        // Mock withMdc to return the same Runnable
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // Mock all master data helper methods to just modify the map for visibility
        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("master", "ok");
            return null;
        }).when(packingV3Util).addAllMasterDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("unlocation", "ok");
            return null;
        }).when(packingV3Util).addAllUnlocationDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("commodity", "ok");
            return null;
        }).when(packingV3Util).addAllCommodityTypesInSingleCall(any(), any());

        Map<String, Object> responseMap = packingV3Service.fetchAllMasterDataByKey(packingResponse);

        // Validate map contains all expected keys
        assertEquals(3, responseMap.size());
        assertEquals("ok", responseMap.get("unlocation"));
        assertEquals("ok", responseMap.get("commodity"));

        verify(packingV3Util).addAllMasterDataInSingleCall(any(), any());
        verify(packingV3Util).addAllUnlocationDataInSingleCall(any(), any());
        verify(packingV3Util).addAllCommodityTypesInSingleCall(any(), any());

    }

    @Test
    void testFetchShipmentPackages_invalidEntityId_shouldThrowException() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setEntityId("");  // or "0"

        ValidationException exception = assertThrows(ValidationException.class, () ->
                packingV3Service.fetchShipmentPackages(listCommonRequest, "xSource")
        );

        assertEquals("Entity id is empty", exception.getMessage());
    }

    @Test
    void testFetchShipmentPackages_withContainers_shouldMapContainerNumbers() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setEntityId("123");
        response.setContainerId(1L);
        response.setContainerNumber("C123");

        ContainerInfoProjection projection = mock(ContainerInfoProjection.class);
        when(projection.getId()).thenReturn(1L);
        when(projection.getContainerNumber()).thenReturn("C123");

        Page<Packing> page = new PageImpl<>(List.of(packing));
        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);
        when(containerV3Service.getContainers(List.of(1L))).thenReturn(List.of(projection));

        PackingAssignmentProjection assignmentProjection = mock(PackingAssignmentProjection.class);
        when(assignmentProjection.getAssignedCount()).thenReturn(1L);
        when(assignmentProjection.getUnassignedCount()).thenReturn(1L);
        when(packingDao.getPackingAssignmentCountByShipment(anyLong())).thenReturn(assignmentProjection);

        PackingListResponse actual = packingV3Service.fetchShipmentPackages(listCommonRequest, "xSource");

        assertEquals("C123", actual.getPackings().get(0).getContainerNumber());
        assertEquals(1L, actual.getAssignedPackageCount());
        assertEquals(1L, actual.getUnassignedPackageCount());
    }

    @Test
    void testFetchShipmentPackages_withoutContainers_shouldSkipMapping() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setEntityId("1");

        PackingResponse response1 = new PackingResponse();
        response1.setContainerId(null);  // No container
        PackingListResponse packingListResponse = new PackingListResponse();
        packingListResponse.setPackings(List.of(response1));

        Page<Packing> page = new PageImpl<>(List.of(packing));
        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(response);
        PackingAssignmentProjection assignmentProjection = mock(PackingAssignmentProjection.class);
        when(assignmentProjection.getAssignedCount()).thenReturn(1L);
        when(assignmentProjection.getUnassignedCount()).thenReturn(2L);
        when(packingDao.getPackingAssignmentCountByShipment(anyLong())).thenReturn(assignmentProjection);

        PackingListResponse actual = packingV3Service.fetchShipmentPackages(listCommonRequest, "xSource");

        assertNull(actual.getPackings().get(0).getContainerNumber()); // container mapping not done
        assertEquals(1L, actual.getAssignedPackageCount());
        assertEquals(2L, actual.getUnassignedPackageCount());
    }

    @Test
    void testCalculatePackSummary() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setShipmentEntityId(14388L);
        when(shipmentService.findById(any())).thenReturn(Optional.of(testShipment));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        PackSummaryV3Response response1 = packingV3Service.calculatePackSummary(request1, null);
        assertNotNull(response1);
    }

    @Test
    void testCalculatePackSummary2() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setShipmentEntityId(14388L);
        when(shipmentService.retrieveForNte(any())).thenReturn(Optional.of(testShipment));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        PackSummaryV3Response response1 = packingV3Service.calculatePackSummary(request1, Constants.NETWORK_TRANSFER);
        assertNotNull(response1);
    }

    @Test
    void testCalculatePackSummary3() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setShipmentEntityId(14388L);
        when(shipmentService.retrieveForNte(any())).thenReturn(Optional.empty());
        assertThrows(IllegalArgumentException.class, () -> packingV3Service.calculatePackSummary(request1, Constants.NETWORK_TRANSFER));
    }

    @Test
    void testCalculatePackSummary4() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setConsolidationId(14388L);
        when(consolidationService.findById(any())).thenReturn(Optional.of(testconsol));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        PackSummaryV3Response response1 = packingV3Service.calculatePackSummary(request1, null);
        assertNotNull(response1);
    }

    @Test
    void testCalculatePackSummary5() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setConsolidationId(14388L);
        when(consolidationService.retrieveForNte(any())).thenReturn(Optional.of(testconsol));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        PackSummaryV3Response response1 = packingV3Service.calculatePackSummary(request1, Constants.NETWORK_TRANSFER);
        assertNotNull(response1);
    }

    @Test
    void testCalculatePackSummary6() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setConsolidationId(14388L);
        when(consolidationService.retrieveForNte(any())).thenReturn(Optional.empty());
        assertThrows(IllegalArgumentException.class, () -> packingV3Service.calculatePackSummary(request1, Constants.NETWORK_TRANSFER));
    }

    @Test
    void testUnAssignPackageContainers() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        packingV3Service.unAssignPackageContainers(request);
        verify(containerV3Service, never()).unAssignContainers(any(), any());
    }

    @Test
    void testUnAssignPackageContainers1() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        packing = new Packing();
        packing.setId(1L);
        packing.setShipmentId(1L);
        packing.setContainerId(1L);
        when(packingDao.findByIdIn(any())).thenReturn(List.of(packing));
        packingV3Service.unAssignPackageContainers(request);
        verify(containerV3Service).unAssignContainers(any(), any());
    }

    @Test
    void testSetPacksUnits_shouldSetOnlyDgPacksUnit_whenOnlyDgSetHasSingleValue() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        Set<String> uniquePacksUnits = new HashSet<>();
        uniquePacksUnits.add("KG");
        Set<String> dgPacksUnitSet = Set.of("KG");
        packingV3Service.setPacksUnits(cargoDetailsResponse, uniquePacksUnits, dgPacksUnitSet);
        assertEquals("KG",cargoDetailsResponse.getPacksUnit());
        assertEquals("KG", cargoDetailsResponse.getDgPacksUnit(), "dgPacksUnit should be set to 'KG'");
    }
    @Test
    void testSetPacksUnits_shouldSetDgPacksAndTotalPackUnit_whenOnlyDgSetHasSingleValue() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        Set<String> uniquePacksUnits = new HashSet<>();
        uniquePacksUnits.add("KG");
        Set<String> dgPacksUnitSet = Set.of("KG");
        packingV3Service.setPacksUnits(cargoDetailsResponse, uniquePacksUnits, dgPacksUnitSet);
        assertNotNull(cargoDetailsResponse.getPacksUnit());
        assertEquals("KG", cargoDetailsResponse.getDgPacksUnit(), "dgPacksUnit should be set to 'KG'");
    }

    @Test
    void testUpdateOceanDGStatus_shouldReturnEarly_whenShipmentDetailsIsNull() throws RunnerException {
        List<Packing> oldPackings = createMockPackings();
        List<Packing> updatedPackings = createMockPackings();

        packingV3Service.updateOceanDGStatus(null, oldPackings, updatedPackings);

        verifyNoInteractions(commonUtils);
    }

    @Test
    void testUpdateOceanDGStatus_shouldReturnEarly_whenUpdatedPackingsIsNull() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        List<Packing> oldPackings = createMockPackings();

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, null);

        verifyNoInteractions(commonUtils);
    }

    @Test
    void testUpdateOceanDGStatus_shouldReturnEarly_whenUpdatedPackingsIsEmpty() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        List<Packing> oldPackings = createMockPackings();
        List<Packing> updatedPackings = new ArrayList<>();

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verifyNoInteractions(commonUtils);
    }

    @Test
    void testUpdateOceanDGStatus_shouldReturnEarly_whenTransportModeIsNotSea() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_AIR);
        List<Packing> oldPackings = createMockPackings();
        List<Packing> updatedPackings = createMockPackings();

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verifyNoInteractions(commonUtils);
    }

    @Test
    void testUpdateOceanDGStatus_shouldProcessNewPacking_whenOldPackingNotExistsAndHasDGClass() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        List<Packing> oldPackings = new ArrayList<>();
        List<Packing> updatedPackings = Arrays.asList(createPacking(1L, "DG_CLASS_2"));
        when(commonUtils.checkIfAnyDGClass("DG_CLASS_2")).thenReturn(true);
        when(commonUtils.checkIfDGClass1("DG_CLASS_2")).thenReturn(false);

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verify(commonUtils).checkIfAnyDGClass("DG_CLASS_2");
        verify(commonUtils).checkIfDGClass1("DG_CLASS_2");
    }

    @Test
    void testUpdateOceanDGStatus_shouldProcessNewPacking_whenOldPackingNotExistsAndHasDGClass1() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        List<Packing> oldPackings = new ArrayList<>();
        List<Packing> updatedPackings = Arrays.asList(createPacking(1L, "DG_CLASS_1"));
        when(commonUtils.checkIfAnyDGClass("DG_CLASS_1")).thenReturn(true);
        when(commonUtils.checkIfDGClass1("DG_CLASS_1")).thenReturn(true);

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verify(commonUtils).checkIfAnyDGClass("DG_CLASS_1");
        verify(commonUtils).checkIfDGClass1("DG_CLASS_1");
    }

    @Test
    void testUpdateOceanDGStatus_shouldNotProcessNewPacking_whenOldPackingNotExistsAndNoDGClass() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        List<Packing> oldPackings = new ArrayList<>();
        List<Packing> updatedPackings = Arrays.asList(createPacking(1L, "NO_DG_CLASS"));
        when(commonUtils.checkIfAnyDGClass("NO_DG_CLASS")).thenReturn(false);

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verify(commonUtils).checkIfAnyDGClass("NO_DG_CLASS");
        verify(commonUtils, never()).checkIfDGClass1(anyString());
    }

    @Test
    void testUpdateOceanDGStatus_shouldProcessExistingPacking_whenDGFieldsChanged() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        Packing oldPacking = createPacking(1L, "OLD_DG_CLASS");
        Packing updatedPacking = createPacking(1L, "NEW_DG_CLASS");
        List<Packing> oldPackings = Arrays.asList(oldPacking);
        List<Packing> updatedPackings = Arrays.asList(updatedPacking);
        when(commonUtils.checkIfDGFieldsChangedInPackingV3(updatedPacking, oldPacking)).thenReturn(true);
        when(commonUtils.checkIfDGClass1("NEW_DG_CLASS")).thenReturn(false);

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verify(commonUtils).checkIfDGFieldsChangedInPackingV3(updatedPacking, oldPacking);
        verify(commonUtils).checkIfDGClass1("NEW_DG_CLASS");
    }

    @Test
    void testUpdateOceanDGStatus_shouldNotProcessExistingPacking_whenDGFieldsNotChanged() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        Packing oldPacking = createPacking(1L, "DG_CLASS");
        Packing updatedPacking = createPacking(1L, "DG_CLASS");
        List<Packing> oldPackings = Arrays.asList(oldPacking);
        List<Packing> updatedPackings = Arrays.asList(updatedPacking);
        when(commonUtils.checkIfDGFieldsChangedInPackingV3(updatedPacking, oldPacking)).thenReturn(false);

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verify(commonUtils).checkIfDGFieldsChangedInPackingV3(updatedPacking, oldPacking);
        verify(commonUtils, never()).checkIfDGClass1(anyString());
    }

    @Test
    void testUpdateOceanDGStatus_shouldHandleMultiplePackings_withMixedScenarios() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        Packing oldPacking1 = createPacking(1L, "OLD_DG_CLASS");
        List<Packing> oldPackings = Arrays.asList(oldPacking1);
        Packing updatedPacking1 = createPacking(1L, "UPDATED_DG_CLASS");
        Packing updatedPacking2 = createPacking(2L, "1");
        List<Packing> updatedPackings = Arrays.asList(updatedPacking1, updatedPacking2);
        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);
        assertNotNull(shipmentDetails);

    }

    @Test
    void testUpdateOceanDGStatus_shouldSkipPackingsWithNullIds_whenProcessingUpdatedPackings() throws RunnerException {
        ShipmentDetails shipmentDetails = createShipmentDetails(TRANSPORT_MODE_SEA);
        List<Packing> oldPackings = new ArrayList<>();
        Packing validPacking = createPacking(1L, "DG_CLASS");
        Packing nullIdPacking = createPacking(null, "DG_CLASS");
        List<Packing> updatedPackings = Arrays.asList(validPacking, nullIdPacking);
        when(commonUtils.checkIfAnyDGClass("DG_CLASS")).thenReturn(true);
        when(commonUtils.checkIfDGClass1("DG_CLASS")).thenReturn(false);

        packingV3Service.updateOceanDGStatus(shipmentDetails, oldPackings, updatedPackings);

        verify(commonUtils, times(1)).checkIfAnyDGClass("DG_CLASS");
        verify(commonUtils, times(1)).checkIfDGClass1("DG_CLASS");
    }

    // Helper methods
    private ShipmentDetails createShipmentDetails(String transportMode) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(transportMode);
        return shipmentDetails;
    }

    private Packing createPacking(Long id, String dgClass) {
        packing = new Packing();
        packing.setId(id);
        packing.setDGClass(dgClass);
        return packing;
    }

    private List<Packing> createMockPackings() {
        return Arrays.asList(
                createPacking(1L, "DG_CLASS_1"),
                createPacking(2L, "DG_CLASS_2")
        );
    }

    @Test
    void testupdateOceanDGStatus() throws RunnerException {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        Packing packing1 = new Packing();
        packing1.setId(1L);
        packing1.setDGClass("1L");
        List<Packing> updatedPackings = List.of(packing1);
        when(commonUtils.checkIfAnyDGClass(anyString())).thenReturn(true);
        when(commonUtils.changeShipmentDGStatusToReqd(any(), anyBoolean())).thenReturn(true);
        packingV3Service.updateOceanDGStatus(shipmentDetails, null, updatedPackings);
        verify(commonUtils, times(1)).changeShipmentDGStatusToReqd(any(), anyBoolean());
    }
}
