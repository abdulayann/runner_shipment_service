package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryV3Response;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
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
import org.apache.commons.lang3.StringUtils;
import org.apache.http.auth.AuthenticationException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_CONTAINER_FIELDS_VALIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_PACKING;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class})
@Execution(CONCURRENT)
class PackingV3ServiceTest extends CommonMocks {

    @Spy
    @InjectMocks
    private PackingV3Service packingV3Service;

    @Mock
    private IPackingDao packingDao;
    @Mock
    private IContainerDao containerDao;
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
    @Mock
    private ConsolidationV3Service consolidationV3Service;

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
        request.setIsTemperatureControlled(Boolean.FALSE);
        request.setHazardous(Boolean.FALSE);
        request.setCommodity("Community");

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
        when(consolidationV3Service.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
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
        when(consolidationV3Service.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
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
        when(consolidationV3Service.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
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

        BulkPackingResponse result = packingV3Service.updateBulk(requestList, "SHIPMENT", true);

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

        BulkPackingResponse result = packingV3Service.updateBulk(requestList, "CONSOLIDATION", false);

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

        BulkPackingResponse result = packingV3Service.updateBulk(requestList, "SHIPMENT", true);

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

        PackingListResponse actualResponse = packingV3Service.list(listCommonRequest, true, null, Constants.SHIPMENT);

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

        PackingListResponse actualResponse = packingV3Service.list(listCommonRequest, false, Constants.NETWORK_TRANSFER, Constants.SHIPMENT);

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
        assertThrows(ValidationException.class, () -> packingV3Service.list(null, false, null, Constants.SHIPMENT));
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
        when(consolidationV3Service.findById(any())).thenReturn(Optional.of(testconsol));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        PackSummaryV3Response response1 = packingV3Service.calculatePackSummary(request1, null);
        assertNotNull(response1);
    }

    @Test
    void testCalculatePackSummary5() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setConsolidationId(14388L);
        when(consolidationV3Service.retrieveForNte(any())).thenReturn(Optional.of(testconsol));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        PackSummaryV3Response response1 = packingV3Service.calculatePackSummary(request1, Constants.NETWORK_TRANSFER);
        assertNotNull(response1);
    }

    @Test
    void testCalculatePackSummary6() {
        CalculatePackSummaryRequest request1 = new CalculatePackSummaryRequest();
        request1.setConsolidationId(14388L);
        assertThrows(IllegalArgumentException.class, () -> packingV3Service.calculatePackSummary(request1, Constants.NETWORK_TRANSFER));
    }

    @Test
    void testUnAssignPackageContainers() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        packingV3Service.unAssignPackageContainers(request, Constants.CONSOLIDATION_PACKING);
        verify(containerV3Service, never()).unAssignContainers(any(), any(), any(), any(), any(), any(), anyBoolean(), anyBoolean());
    }

    @Test
    void testUnAssignPackageContainersWithReassignFlow() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        packingV3Service.unAssignPackageContainers(request, Constants.CONSOLIDATION_PACKING);
        verify(containerV3Service, never()).unAssignContainers(any(), any(), any(), any(), any(), any(), anyBoolean(), anyBoolean());
    }

    @Test
    void testUnAssignPackageContainers1() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        packing = new Packing();
        packing.setId(1L);
        packing.setShipmentId(1L);
        packing.setContainerId(1L);
        when(packingDao.findByIdIn(any())).thenReturn(List.of(packing));
        packingV3Service.unAssignPackageContainers(request, Constants.CONSOLIDATION_PACKING);
        verify(containerV3Service).unAssignContainers(any(), any(), any(), any(), any(), any(), anyBoolean(), anyBoolean());
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

    @Test
    void testAssignShipmentPackagesContainers_success() throws RunnerException {

        AssignContainerRequest request = new AssignContainerRequest();
        request.setContainerId(100L);
        request.setShipmentPackIds(Map.of(10L, List.of(1L, 2L)));

        Packing packing1 = new Packing();
        packing1.setId(1L);
        packing1.setShipmentId(10L);

        Packing packing2 = new Packing();
        packing2.setId(2L);
        packing2.setShipmentId(10L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(10L);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);

        ContainerResponse expectedResponse = new ContainerResponse();
        expectedResponse.setContainerCode("C123");

        when(packingDao.findByIdIn(List.of(1L, 2L))).thenReturn(List.of(packing1, packing2));
        when(shipmentService.findById(10L)).thenReturn(Optional.of(shipmentDetails));
        when(containerV3Service.assignContainers(request, SHIPMENT_PACKING, Boolean.TRUE))
                .thenReturn(expectedResponse);

        ContainerResponse response = packingV3Service.assignShipmentPackagesContainers(request);

        assertNotNull(response);
        assertEquals(expectedResponse, response);

        verify(packingDao).findByIdIn(List.of(1L, 2L));
        verify(shipmentService).findById(10L);
        verify(containerV3Service).assignContainers(request, SHIPMENT_PACKING, Boolean.TRUE);
    }

    @Test
    void testAssignShipmentPackagesContainers_noPackingIdsProvided_shouldThrowValidationException() {
        ShipmentPackAssignmentRequest request = new ShipmentPackAssignmentRequest();
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(request.getContainerId());
        assignContainerRequest.setShipmentPackIds(new HashMap<>());
        request.setPackingIds(Collections.emptyList());
        ValidationException exception = assertThrows(ValidationException.class, () -> packingV3Service.assignShipmentPackagesContainers(assignContainerRequest));
        assertEquals("No Shipment/Packing Ids provided.", exception.getMessage());
    }

    @Test
    void testAssignShipmentPackagesContainers_noPackingFound_shouldThrowValidationException() {
        ShipmentPackAssignmentRequest request = new ShipmentPackAssignmentRequest();
        request.setPackingIds(List.of(1L, 2L));
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(request.getContainerId());
        assignContainerRequest.setShipmentPackIds(Map.of(10L, request.getPackingIds()));
        when(packingDao.findByIdIn(request.getPackingIds())).thenReturn(Collections.emptyList());
        ValidationException exception = assertThrows(ValidationException.class, () -> packingV3Service.assignShipmentPackagesContainers(assignContainerRequest));
        assertEquals("No Packing found with Ids: [1, 2]", exception.getMessage());
    }

    @Test
    void testAssignShipmentPackagesContainers_multipleShipmentIds_shouldThrowValidationException() {
        ShipmentPackAssignmentRequest request = new ShipmentPackAssignmentRequest();
        request.setPackingIds(List.of(1L, 2L));
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(request.getContainerId());
        assignContainerRequest.setShipmentPackIds(Map.of(10L, request.getPackingIds()));
        Packing packing1 = new Packing();
        packing1.setId(1L);
        packing1.setShipmentId(100L);

        Packing packing2 = new Packing();
        packing2.setId(2L);
        packing2.setShipmentId(200L);
          List<Packing> packingList = List.of(
               packing1, packing2 // Shipment ID 200
        );
        when(packingDao.findByIdIn(request.getPackingIds())).thenReturn(packingList);
        ValidationException exception = assertThrows(ValidationException.class, () -> packingV3Service.assignShipmentPackagesContainers(assignContainerRequest));
        assertEquals("Please select Packages of single shipment only for assignment.", exception.getMessage());
    }

    @Test
    void testAssignShipmentPackagesContainers_invalidShipmentType_shouldThrowValidationException() {
        ShipmentPackAssignmentRequest request = new ShipmentPackAssignmentRequest();
        request.setPackingIds(List.of(1L));
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(request.getContainerId());
        assignContainerRequest.setShipmentPackIds(Map.of(10L, request.getPackingIds()));
        Packing packing = new Packing();
        packing.setId(1L);
        packing.setShipmentId(100L); // Shipment ID 100
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentType("LCL"); // Invalid type
        when(packingDao.findByIdIn(request.getPackingIds())).thenReturn(List.of(packing));
        when(shipmentService.findById(100L)).thenReturn(Optional.of(shipmentDetails));
        ValidationException exception = assertThrows(ValidationException.class, () -> packingV3Service.assignShipmentPackagesContainers(assignContainerRequest));
        assertEquals("Shipment level package assignment is only allowed for FCL/FTL shipments.", exception.getMessage());
    }

    @Test
    void testUnAssignPackageContainers_nullContainerId_shouldThrowValidationException() {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        request.setPackingIds(List.of(1L, 2L));
        Packing packing1 = new Packing();
        packing1.setId(1L);
        packing1.setContainerId(null);
        Packing packing2 = new Packing();
        packing2.setId(2L);
        packing2.setContainerId(100L);

        when(packingDao.findByIdIn(request.getPackingIds())).thenReturn(List.of(packing1, packing2));

        ValidationException exception = assertThrows(ValidationException.class, () -> {
            packingV3Service.unAssignPackageContainers(request, Constants.CONTAINER);
        });

        assertEquals("Container Id is null for packing with Id: 1", exception.getMessage());
    }

    @Test
    void testUnAssignPackageContainers_multipleShipmentIds_shouldThrowValidationException() {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        request.setPackingIds(List.of(1L, 2L));
        Packing packing1 = new Packing();
        packing1.setId(1L);
        packing1.setContainerId(100L);
        packing1.setShipmentId(10L);
        Packing packing2 = new Packing();
        packing2.setId(2L);
        packing2.setContainerId(101L);
        packing2.setShipmentId(20L);

        when(packingDao.findByIdIn(request.getPackingIds())).thenReturn(List.of(packing1, packing2));

        ValidationException exception = assertThrows(ValidationException.class, () -> {
            packingV3Service.unAssignPackageContainers(request, Constants.CONTAINER);
        });

        assertEquals("Please select Packages of only one shipment for unAssign action", exception.getMessage());
    }

    @Test
    void testUnAssignPackageContainers_validRequest_shouldProcessSuccessfully() {
        UnAssignPackageContainerRequest unAssignedPackageContainerRequest = new UnAssignPackageContainerRequest();
        unAssignedPackageContainerRequest.setPackingIds(List.of(1L, 2L));
        Packing packing1 = new Packing();
        packing1.setId(1L);
        packing1.setContainerId(100L);
        packing1.setShipmentId(10L);
        Packing packing2 = new Packing();
        packing2.setId(2L);
        packing2.setContainerId(100L);
        packing2.setShipmentId(10L);

        when(packingDao.findByIdIn(unAssignedPackageContainerRequest.getPackingIds())).thenReturn(List.of(packing1, packing2));

        assertDoesNotThrow(() -> packingV3Service.unAssignPackageContainers(unAssignedPackageContainerRequest, Constants.CONTAINER));
    }

    @Test
    void testUpdateShipmentAndContainerDataForFCLAndFTLShipments_WithValidShipments_ShouldUpdateSummaries() throws RunnerException {
        Long shipmentId1 = 100L;
        Long shipmentId2 = 200L;
        ShipmentDetails shipment1 = new ShipmentDetails();
        ShipmentDetails shipment2 = new ShipmentDetails();
        Map<Long, ShipmentDetails> shipmentMap = Map.of(
                shipmentId1, shipment1,
                shipmentId2, shipment2
        );
        Set<Long> fclOrFtlIds = Set.of(shipmentId1, shipmentId2);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        UnAssignContainerParams params = new UnAssignContainerParams();
        params.setFclOrFtlShipmentIds(fclOrFtlIds);
        params.setShipmentDetailsMap(shipmentMap);
        params.setConsolidationDetails(consolidationDetails);
        packingV3Service.updateShipmentAndContainerDataForFCLAndFTLShipments(params);
        ArgumentCaptor<ShipmentDetails> captor = ArgumentCaptor.forClass(ShipmentDetails.class);
        verify(shipmentService, times(2)).calculateAndUpdateShipmentCargoSummary(captor.capture());
        List<ShipmentDetails> capturedArgs = captor.getAllValues();
        assertTrue(capturedArgs.contains(shipment1));
        assertTrue(capturedArgs.contains(shipment2));
    }

    @Test
    void testUpdateShipmentAndContainerDataForFCLAndFTLShipments_WithNullIds_ShouldDoNothing() throws RunnerException {
        UnAssignContainerParams params = new UnAssignContainerParams();
        params.setFclOrFtlShipmentIds(null);
        packingV3Service.updateShipmentAndContainerDataForFCLAndFTLShipments(params);
        Mockito.verifyNoInteractions(shipmentService, consolidationV3Service);
    }

    @Test
    void testUpdateShipmentAndContainerDataForFCLAndFTLShipments_WithEmptyIds_ShouldDoNothing() throws RunnerException {
        UnAssignContainerParams params = new UnAssignContainerParams();
        params.setFclOrFtlShipmentIds(Collections.emptySet());
        packingV3Service.updateShipmentAndContainerDataForFCLAndFTLShipments(params);
        Mockito.verifyNoInteractions(shipmentService, consolidationV3Service);
    }

    @Test
    void testAssignPackagesContainers_WhenMultipleShipmentIds_ThrowsValidationException() {
        AssignContainerRequest assignedContainerRequest = new AssignContainerRequest();
        assignedContainerRequest.setShipmentPackIds(Map.of(1L, List.of(100L), 2L, List.of(200L)));
        ValidationException ex = assertThrows(
                ValidationException.class,
                () -> packingV3Service.assignPackagesContainers(assignedContainerRequest)
        );
        assertEquals("Please select Packages of single shipment only for assignment.", ex.getMessage());
    }

    @Test
    void assignPackagesContainers_multipleShipments_throwsValidationException() {
        AssignContainerRequest request = new AssignContainerRequest();
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        shipmentPackIds.put(1L, Arrays.asList(101L));
        shipmentPackIds.put(2L, Arrays.asList(201L));
        request.setShipmentPackIds(shipmentPackIds);

        assertThrows(ValidationException.class,
                () -> packingV3Service.assignPackagesContainers(request));
    }

    @Test
    void assignPackagesContainers_singleShipment_delegatesToAssignContainers() throws RunnerException {
        AssignContainerRequest request = new AssignContainerRequest();
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        shipmentPackIds.put(1L, Arrays.asList(101L, 102L));
        request.setShipmentPackIds(shipmentPackIds);

        ContainerResponse expectedResponse = new ContainerResponse();
        expectedResponse.setContainerCode("C123");

        when(containerV3Service.assignContainers(request, Constants.CONSOLIDATION_PACKING, Boolean.TRUE))
                .thenReturn(expectedResponse);

        ContainerResponse actualResponse = packingV3Service.assignPackagesContainers(request);

        assertEquals(expectedResponse, actualResponse);
        verify(containerV3Service).assignContainers(request, Constants.CONSOLIDATION_PACKING, Boolean.TRUE);
    }


    @Test
    void testCalculateCargoSummary_FCL() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(123L).build();
        ShipmentDetails shipmentDetail = new ShipmentDetails();
        shipmentDetail.setDirection(DIRECTION_EXP);
        shipmentDetail.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetail.setShipmentType(CARGO_TYPE_FCL);
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setNoOfPacks(23);

        when(commonUtils.isSeaFCLOrRoadFTL(TRANSPORT_MODE_SEA, CARGO_TYPE_FCL)).thenReturn(true);
        when(shipmentService.calculateShipmentSummary(anyString(), any(), any())).thenReturn(cargoDetailsResponse);
        when(shipmentDao.findById(123L)).thenReturn(Optional.of(shipmentDetail));
        var resp = packingV3Service.calculateCargoSummary(commonGetRequest);
        assertEquals(23, resp.getNoOfPacks());

    }

    @Test
    void testCalculateCargoSummary_LCL() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(123L).build();
        ShipmentDetails shipmentDetail = new ShipmentDetails();
        shipmentDetail.setDirection(DIRECTION_EXP);
        shipmentDetail.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetail.setShipmentType(CARGO_TYPE_LCL);
        shipmentDetail.setPackingList(List.of(new Packing()));
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setNoOfPacks(23);

        when(consolidationV3Service.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(new VolumeWeightChargeable());
        when(commonUtils.isSeaFCLOrRoadFTL(TRANSPORT_MODE_SEA, CARGO_TYPE_LCL)).thenReturn(false);
        when(shipmentDao.findById(123L)).thenReturn(Optional.of(shipmentDetail));
        var resp = packingV3Service.calculateCargoSummary(commonGetRequest);
        assertEquals(TRANSPORT_MODE_SEA, resp.getTransportMode());
    }

    @Test
    void list_shouldHandleContainerSearchAndSetFilterCriteria() {
        ListCommonRequest request = new ListCommonRequest();
        request.setEntityId("123"); // String value that will be converted to Long
        request.setContainsText("CONTAINER123");
        when(commonUtils.getLongValue("123")).thenReturn(123L);
        when(packingDao.getContainerIdByContainerNumberAndType("CONTAINER123", 123L, "SHIPMENT"))
                .thenReturn(Arrays.asList(1L, 2L, 3L));
        Page<Packing> mockPage = new PageImpl<>(Collections.emptyList());
        when(packingDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(mockPage);
        PackingListResponse response = packingV3Service.list(request, true, "WEB", "SHIPMENT");
        assertNotNull(response);
        assertEquals(StringUtils.EMPTY, request.getContainsText());
        assertNotNull(request.getFilterCriteria());
        assertEquals(1, request.getFilterCriteria().size());
        FilterCriteria filter = request.getFilterCriteria().get(0);
        Criteria containerCriteria = filter.getInnerFilter().get(0).getCriteria();
        assertEquals("containerId", containerCriteria.getFieldName());
        assertEquals("IN", containerCriteria.getOperator());
        assertEquals(Arrays.asList(1L, 2L, 3L), containerCriteria.getValue());
    }

    @Test
    void list_shouldSetFilterWhenContainerIdsFound() {
        ListCommonRequest request = new ListCommonRequest();
        request.setEntityId("123");
        request.setContainsText("CONTAINER123");
        when(commonUtils.getLongValue("123")).thenReturn(123L);
        when(packingDao.getContainerIdByContainerNumberAndType("CONTAINER123", 123L, "SHIPMENT"))
                .thenReturn(Arrays.asList(1L, 2L, 3L));
        Page<Packing> mockPage = new PageImpl<>(Collections.emptyList());
        when(packingDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(mockPage);
        packingV3Service.list(request, true, "WEB", "SHIPMENT");
        assertEquals(StringUtils.EMPTY, request.getContainsText()); // Now this should pass
        assertNotNull(request.getFilterCriteria());
        assertEquals(1, request.getFilterCriteria().size());
        FilterCriteria filter = request.getFilterCriteria().get(0);
        Criteria containerCriteria = filter.getInnerFilter().get(0).getCriteria();
        assertEquals("containerId", containerCriteria.getFieldName());
        assertEquals("IN", containerCriteria.getOperator());
        assertEquals(Arrays.asList(1L, 2L, 3L), containerCriteria.getValue());
    }

    @Test
    void list_shouldThrowValidationExceptionForNullRequest() {
        assertThrows(ValidationException.class,
                () -> packingV3Service.list(null, true, "WEB", "SHIPMENT"));
    }
    @Test
    void testUpdateBulk_FCLError() {
        List<PackingV3Request> requestList = List.of(request);
        testShipment.setDirection(null);
        testShipment.setShipmentType("FCL");
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setOpenForAttachment(false);
        testShipment.setConsolidationList(Set.of(consolidationDetails));
        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));
        when(commonUtils.isFCL(anyString())).thenReturn(true);
        when(packingValidationV3Util.validateModule(any(), anyString())).thenReturn(testShipment);

        assertThrows(ValidationException.class, () ->  packingV3Service.updateBulk(requestList, "SHIPMENT", false));
    }
    @Test
    void testUpdateBulk_LCLError() {
        List<PackingV3Request> requestList = List.of(request);
        testShipment.setDirection(null);
        testShipment.setShipmentType("LCL");
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setOpenForAttachment(false);
        testShipment.setConsolidationList(Set.of(consolidationDetails));
        when(packingDao.findByIdIn(anyList())).thenReturn(List.of(packing));
        when(commonUtils.isLCL(anyString())).thenReturn(true);
        when(packingValidationV3Util.validateModule(any(), anyString())).thenReturn(testShipment);

        assertThrows(ValidationException.class, () ->  packingV3Service.updateBulk(requestList, "SHIPMENT", false));
    }

    @Test
    void testAddDGValidation_NoValidationTriggered_DGClassNotAdded() {
        Packing oldPacking = new Packing();
        oldPacking.setDGClass("EXISTING");

        Packing updatedPacking = new Packing();
        updatedPacking.setDGClass("NEW");
        updatedPacking.setContainerId(100L);

        Containers container = new Containers();
        container.setDgClass(null);

        when(containerDao.findById(100L)).thenReturn(Optional.of(container));

        Map<Long, Packing> oldPackingMap = Map.of(1L, oldPacking);
        Map<Long, Packing> updatedPackingMap = Map.of(1L, updatedPacking);
        Set<Long> requestIds = new HashSet<>(Set.of(1L));

        packingV3Service.addDGValidation(oldPackingMap, updatedPackingMap, requestIds);

        // Verify that the container was queried
        verify(containerDao).findById(100L);
    }

    @Test
    void testAddDGValidation_ValidationTriggered() {
        Packing oldPacking = new Packing();
        Packing updatedPacking = new Packing();
        updatedPacking.setDGClass("NEW");
        updatedPacking.setContainerId(200L);

        Containers container = new Containers();
        container.setDgClass(null);
        container.setUnNumber(null);
        container.setProperShippingName(null);

        when(containerDao.findById(200L)).thenReturn(Optional.of(container));
        Map<Long, Packing> oldPackingMap = Map.of(1L, oldPacking);
        Map<Long, Packing> updatedPackingMap = Map.of(1L, updatedPacking);
        Set<Long> requestIds = new HashSet<>(Set.of(1L));

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingV3Service.addDGValidation(oldPackingMap, updatedPackingMap, requestIds)
        );

        assertEquals(OCEAN_DG_CONTAINER_FIELDS_VALIDATION, ex.getMessage());
    }

    @Test
    void testAddDGValidation_DGClassAddedButValidContainerFields() {
        Packing oldPacking = new Packing();
        Packing updatedPacking = new Packing();
        updatedPacking.setDGClass("NEW");
        updatedPacking.setContainerId(300L);

        Containers container = new Containers();
        container.setDgClass("DGC");
        container.setUnNumber("UN1234");
        container.setProperShippingName("Some Name");

        when(containerDao.findById(300L)).thenReturn(Optional.of(container));

        Map<Long, Packing> oldPackingMap = Map.of(1L, oldPacking);
        Map<Long, Packing> updatedPackingMap = Map.of(1L, updatedPacking);
        Set<Long> requestIds = new HashSet<>(Set.of(1L));

        packingV3Service.addDGValidation(oldPackingMap, updatedPackingMap, requestIds);

        // Verify that the container was queried and no exception was thrown
        verify(containerDao).findById(300L);
    }

    @Test
    void testAddDGValidation_ContainerNotFound() {
        Packing oldPacking = new Packing();
        oldPacking.setDGClass(null);

        Packing updatedPacking = new Packing();
        updatedPacking.setDGClass("NEW");
        updatedPacking.setContainerId(400L);

        when(containerDao.findById(400L)).thenReturn(Optional.empty());

        Map<Long, Packing> oldPackingMap = Map.of(1L, oldPacking);
        Map<Long, Packing> updatedPackingMap = Map.of(1L, updatedPacking);
        Set<Long> requestIds = new HashSet<>(Set.of(1L));

       assertThrows(ValidationException.class, () ->
                packingV3Service.addDGValidation(oldPackingMap, updatedPackingMap, requestIds)
        );

        // Verify that the container lookup was attempted
        verify(containerDao).findById(400L);
    }
    @Test
    void testAirModeWithNullWeight_shouldSkip() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        cargoDetailsResponse.setShipmentType("ANY");

        Packing packing1 = new Packing();
        Packing packing2 = new Packing();
        packing1.setWeight(BigDecimal.TEN);

        List<Packing> packings = Arrays.asList(packing1, packing2);

        boolean result = packingV3Service.isSkipWeightInCalculation(packings, cargoDetailsResponse, false);

        assertTrue(result, "Should skip weight when AIR and any packing has null weight");
    }

    @Test
    void testSeaModeWithLCLAndNullWeight_shouldSkip() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        cargoDetailsResponse.setShipmentType(Constants.CARGO_TYPE_LCL);

        Packing packing1 = new Packing();

        List<Packing> packings = Collections.singletonList(packing1);

        boolean result = packingV3Service.isSkipWeightInCalculation(packings, cargoDetailsResponse, false);

        assertTrue(result, "Should skip weight when SEA + LCL and null weight exists");
    }

    @Test
    void testSeaModeWithLCLAndAllWeightsPresent_shouldNotSkip() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        cargoDetailsResponse.setShipmentType(Constants.CARGO_TYPE_LCL);

        Packing packing1 = new Packing();
        packing1.setWeight(BigDecimal.TEN);
        Packing packing2 = new Packing();
        packing2.setWeight(BigDecimal.ONE);

        List<Packing> packings = Arrays.asList(packing1, packing2);

        boolean result = packingV3Service.isSkipWeightInCalculation(packings, cargoDetailsResponse, false);

        assertFalse(result, "Should not skip when SEA + LCL but all weights present");
    }

    @Test
    void testRoadModeWithLTLAndNullWeight_shouldSkip() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        cargoDetailsResponse.setShipmentType(Constants.CARGO_TYPE_LTL);

        Packing packing1 = new Packing();

        List<Packing> packings = Collections.singletonList(packing1);

        boolean result = packingV3Service.isSkipWeightInCalculation(packings, cargoDetailsResponse, false);

        assertTrue(result, "Should skip weight when ROAD + LTL and null weight exists");
    }

    @Test
    void testOtherModes_shouldRespectInitialFlag() {
        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
        cargoDetailsResponse.setTransportMode("OTHER_MODE");
        cargoDetailsResponse.setShipmentType("OTHER");

        Packing packing1 = new Packing();

        List<Packing> packings = Collections.singletonList(packing1);

        boolean result = packingV3Service.isSkipWeightInCalculation(packings, cargoDetailsResponse, true);

        assertTrue(result, "Should keep initial flag when mode/type not in condition");
    }
}
