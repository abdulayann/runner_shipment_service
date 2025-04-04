package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.LicenseContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AutoCalculatePackingResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PackingServiceTest extends CommonMocks {

    @Mock
    IPackingDao packingDao;

    @Mock
    @Lazy
    private IConsolidationService consolidationService;

    @Mock
    IContainerDao containersDao;
    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDao;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IPackingSync packingSync;

    @Mock
    private SyncConfig syncConfig;

    @Mock
    private IContainerService containerService;

    @Mock
    private CSVParsingUtil<Packing> parser;
    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private HttpServletResponse response;
    @InjectMocks
    private PackingService packingService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private static PackingResponse packingResponse;
    private static Containers testContainer;
    private static Packing testPacking;
    private static Packing testCsvPacking;
    private static PackingRequest packingRequest;
    private static ShipmentDetails testShipment;
    private static List<Packing> testPackingList;
    private static AutoCalculatePackingRequest testAutoCalculatePackingRequest;
    private static AutoCalculatePackingResponse testAutoCalculatePackingResponse;
    private static PackingRequestV2 testPackingRequestV2;
    private static ConsolidationDetails testConsolidation;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testPacking = jsonTestUtility.getTestPacking();
        testAutoCalculatePackingRequest = objectMapperTest.convertValue(testPacking, AutoCalculatePackingRequest.class);
        testAutoCalculatePackingResponse = objectMapperTest.convertValue(testPacking, AutoCalculatePackingResponse.class);
        testPackingList = jsonTestUtility.getTestPackingList();
        testConsolidation = jsonTestUtility.getTestConsolidation();
        testCsvPacking = jsonTestUtility.getTestCsvPacking();
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        testPackingRequestV2 = jsonTestUtility.getTestPackingRequestV2();
        packingRequest = objectMapperTest.convertValue(testPacking, PackingRequest.class);
        packingResponse = objectMapperTest.convertValue(testPacking, PackingResponse.class);
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void create() {

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(packingRequest);

        ResponseEntity<IRunnerResponse> httpResponse = packingService.create(commonRequestModel);
        assertNull(httpResponse);

    }

    @Test
    void uploadPacking_SuccessWithHazardousNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setDGSubstanceId(null);
        packingList.get(0).setHazardous(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        packingService.uploadPacking(bulkUploadRequest);
        verify(packingDao, times(1)).saveAll(any());
    }

    @Test
    void uploadPacking_InvalidFlashPoint_DgNotNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setHazardous(null);
        packingList.get(0).setFlashPoint("flashPoint");
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        packingService.uploadPacking(bulkUploadRequest);
        verify(packingDao, times(1)).saveAll(any());
    }

    @Test
    void uploadPacking_InvalidFlashPoint() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setDGSubstanceId(null);
        packingList.get(0).setHazardous(null);
        packingList.get(0).setFlashPoint("flashPoint");
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_FlashNotNull() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGSubstanceId(null);
            packingList.get(0).setFlashPoint("flashPoint");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(),
                anyMap(), anyMap())).thenReturn(packingList);
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_UndgContactInvalid() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGSubstanceId(null);
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(),
                anyMap(), anyMap())).thenReturn(packingList);
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_SameCommodityData() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        jsonTestUtility.getMasterDataMap();
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(), anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());
                    return packingList;
                });
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_CommodityTypeInvalid() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        jsonTestUtility.getMasterDataMap();
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(), anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithCommodity());
                    return packingList;
                });
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ChargeableUnitEmpty() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setChargeableUnit("");
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_WtVolNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setWeight(null);
        packingList.get(0).setWeightUnit(null);
        packingList.get(0).setVolume(null);
        packingList.get(0).setVolumeUnit(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(NullPointerException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ChargeableNotKG() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setChargeableUnit(Constants.WEIGHT_UNIT_KT);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_TransportModeSea() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setChargeable(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(NullPointerException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ChargeableNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setChargeable(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(NullPointerException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_TransportModeNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(NullPointerException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ChargeableUnitNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setChargeableUnit(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ChargeableUnitInvalid() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        jsonTestUtility.getMasterDataMap();
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(), anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMap());
                    return packingList;
                });
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_PackVolumeInvalid() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolume(new BigDecimal(321));
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_VolWtNull() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setVolumeWeightUnit("");
            packingList.get(0).setVolumeWeight(null);
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(),
                anyMap(), anyMap())).thenReturn(packingList);
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
            volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_EmptyVolWtUnit() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeWeightUnit("");
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_NullVolWt_VolumetricFunctionTest() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setVolumeWeight(null);
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(),
                anyMap(), anyMap())).thenReturn(packingList);
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
            volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_NullWtVol_vwob_VolumetricFunctionTest() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setWeight(null);
            packingList.get(0).setWeightUnit(null);
            packingList.get(0).setVolume(null);
            packingList.get(0).setVolumeUnit(null);
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(),
                anyMap(), anyMap())).thenReturn(packingList);
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(null);
            volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_NullWtVol_VolumetricFunctionTest() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setWeight(null);
        packingList.get(0).setWeightUnit(null);
        packingList.get(0).setVolume(null);
        packingList.get(0).setVolumeUnit(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_InvalidVolWtUnit() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        volumeWeightChargeable.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KT);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_HazardousFalse() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setDGClass("dgClass1");
        packingList.get(0).setHazardous(false);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(), anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());
                    return packingList;
                });
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        packingService.uploadPacking(bulkUploadRequest);
        verify(packingDao, times(1)).saveAll(any());
    }

    @Test
    void uploadPacking_NullHDgClassMasterData() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass1");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(),
                anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(new HashMap<>());
                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_DiffDgClass() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass1");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(),
                anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());
                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            packingList.get(0).setFlashPoint("23");
            packingList.get(0).setDGSubstanceId(null);
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor2 = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor3 = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(),
                (Map<Long, Long>) captor2.capture(), (Map<Long, String>) captor3.capture(),
                anyMap()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    Map<Integer, Long> dgsubstance = (Map<Integer, Long>) captor2.getValue();
                    dgsubstance.clear();
                    dgsubstance.putAll(jsonTestUtility.getDgSubstanceContactMap());

                    Map<Long, String> dgsubstanceFlashPoint = (Map<Long, String>) captor3.getValue();
                    dgsubstanceFlashPoint.clear();
                    dgsubstanceFlashPoint.putAll(jsonTestUtility.getDgSubstanceFlashPoint());

                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_UndgEmpty() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            packingList.get(0).setFlashPoint("23");
            packingList.get(0).setUNDGContact("");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor2 = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor3 = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(),
                (Map<Long, Long>) captor2.capture(), (Map<Long, String>) captor3.capture(),
                anyMap()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    Map<Long, Long> dgsubstance = (Map<Long, Long>) captor2.getValue();
                    dgsubstance.clear();
                    dgsubstance.putAll(jsonTestUtility.getDgSubstanceContactMap());

                    Map<Long, String> dgsubstanceFlashPoint = (Map<Long, String>) captor3.getValue();
                    dgsubstanceFlashPoint.clear();
                    dgsubstanceFlashPoint.putAll(jsonTestUtility.getDgSubstanceFlashPoint());

                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            UserContext.getUser().getPermissions().put(PermissionConstants.AIR_DG, true);
            packingService.uploadPacking(bulkUploadRequest);
            verify(packingDao, times(1)).saveAll(any());
        }
    }

    @Test
    void uploadPacking_UndgEmpty_DgPermissionError() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(false);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            packingList.get(0).setFlashPoint("23");
            packingList.get(0).setUNDGContact("");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor2 = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor3 = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(),
                (Map<Long, Long>) captor2.capture(), (Map<Long, String>) captor3.capture(),
                anyMap()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    Map<Long, Long> dgsubstance = (Map<Long, Long>) captor2.getValue();
                    dgsubstance.clear();
                    dgsubstance.putAll(jsonTestUtility.getDgSubstanceContactMap());

                    Map<Long, String> dgsubstanceFlashPoint = (Map<Long, String>) captor3.getValue();
                    dgsubstanceFlashPoint.clear();
                    dgsubstanceFlashPoint.putAll(jsonTestUtility.getDgSubstanceFlashPoint());

                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_Exception() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            packingList.get(0).setFlashPoint("23");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor2 = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor3 = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(),
                (Map<Long, Long>) captor2.capture(), (Map<Long, String>) captor3.capture(),
                anyMap()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    Map<Long, Long> dgsubstance = (Map<Long, Long>) captor2.getValue();
                    dgsubstance.clear();
                    dgsubstance.putAll(jsonTestUtility.getDgSubstanceContactMap());

                    Map<Long, String> dgsubstanceFlashPoint = (Map<Long, String>) captor3.getValue();
                    dgsubstanceFlashPoint.clear();
                    dgsubstanceFlashPoint.putAll(jsonTestUtility.getDgSubstanceFlashPoint());

                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_ErrorFlashPointDifferent() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            packingList.get(0).setFlashPoint("flashPoint");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor2 = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor3 = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(),
                (Map<Long, Long>) captor2.capture(), (Map<Long, String>) captor3.capture(),
                anyMap()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    Map<Long, Long> dgsubstance = (Map<Long, Long>) captor2.getValue();
                    dgsubstance.clear();
                    dgsubstance.putAll(jsonTestUtility.getDgSubstanceContactMap());

                    Map<Long, String> dgsubstanceFlashPoint = (Map<Long, String>) captor3.getValue();
                    dgsubstanceFlashPoint.clear();
                    dgsubstanceFlashPoint.putAll(jsonTestUtility.getDgSubstanceFlashPoint());

                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_FlashPointError() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            packingList.get(0).setFlashPoint("flashPoint");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            ArgumentCaptor captor2 = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(),
                (Map<Long, Long>) captor2.capture(), anyMap(), anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());
                    Map<Long, Long> dgsubstance = (Map<Long, Long>) captor2.getValue();
                    dgsubstance.clear();
                    dgsubstance.putAll(jsonTestUtility.getDgSubstanceContactMap());
                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_DgClassMasterData() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            packingList.get(0).setDGClass("dgClass");
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
            when(parser.parseExcelFile(any(), any(), any(),
                (Map<String, Set<String>>) captor.capture(), any(), any(), anyMap(), anyMap(),
                anyMap()))
                .thenAnswer(invocation -> {
                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());
                    return packingList;
                });
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_InvalidDG() throws Exception {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            List<Packing> packingList = List.of(testCsvPacking);
            BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
            bulkUploadRequest.setConsolidationId(1L);
            bulkUploadRequest.setShipmentId(2L);
            bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(),
                anyMap(), anyMap())).thenReturn(packingList);
            VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
            volumeWeightChargeable.setChargeable(new BigDecimal(434));
            volumeWeightChargeable.setVolumeWeight(new BigDecimal("217.167"));
            when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(),
                any())).thenReturn(volumeWeightChargeable);
            assertThrows(ValidationException.class,
                () -> packingService.uploadPacking(bulkUploadRequest));
        }
    }

    @Test
    void uploadPacking_InvalidVolWt() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        volumeWeightChargeable.setVolumeWeight(new BigDecimal("567"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_CommodityNullBranch() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setCommodity(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal("434.01"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ThresholdBranch() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal("434.01"));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_InvalidChargeable() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getVolumeWeightChargeable());
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_InvalidVolumeWeight() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(new BigDecimal(434));
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_VolumeEmpty() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeUnit("");
        packingList.get(0).setVolume(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        assertThrows(NullPointerException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_NullConsolidation(){
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_VolumeUnitEmpty() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeUnit("");
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_VolumeUnitEmpty_VolumeNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolume(null);
        packingList.get(0).setVolumeUnit("");
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(null);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_VolumeNull() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolume(null);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        VolumeWeightChargeable volumeWeightChargeable = jsonTestUtility.getVolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(null);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_VolumeM3Error() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeUnit(Constants.VOLUME_UNIT_CM);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        assertThrows(ValidationException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPacking_ParsingError() throws Exception{
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeUnit(Constants.VOLUME_UNIT_CM);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenThrow(new IOException());
        assertThrows(RunnerException.class, () -> packingService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPackingWeightValueOverAchieved() throws Exception{
        var spyService = Mockito.spy(packingService);
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeUnit(Constants.VOLUME_UNIT_CM);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        AchievedQuantitiesResponse achievedResponse = new AchievedQuantitiesResponse();
        achievedResponse.setWeightUtilization("100.0");
        packSummaryResponse.setConsolidationAchievedQuantities(achievedResponse);

        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        doReturn(packSummaryResponse).when(spyService).calculatePacksUtilisationForConsolidation(any());
        assertThrows(RunnerException.class, () -> spyService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPackingWeightValueOverAchievedSavePackOnOverride() throws Exception{
        var spyService = Mockito.spy(packingService);
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setHazardous(false);
        packingList.get(0).setVolumeUnit(Constants.VOLUME_UNIT_M3);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        bulkUploadRequest.setOverride(true);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        AchievedQuantitiesResponse achievedResponse = new AchievedQuantitiesResponse();
        achievedResponse.setWeightUtilization("100.0");
        packSummaryResponse.setConsolidationAchievedQuantities(achievedResponse);
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setTransportMode("AIR");
        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(new VolumeWeightChargeable());
        doReturn(packSummaryResponse).when(spyService).calculatePacksUtilisationForConsolidation(any());

        spyService.uploadPacking(bulkUploadRequest);

        verify(packingDao, times(1)).saveAll(any());
        verify(packingSync, times(1)).sync(any(), any(), any());
    }

    @Test
    void uploadPackingVolumeValueOverAchieved() throws Exception{
        var spyService = Mockito.spy(packingService);
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setVolumeUnit(Constants.VOLUME_UNIT_CM);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        AchievedQuantitiesResponse achievedResponse = new AchievedQuantitiesResponse();
        achievedResponse.setVolumeUtilization("100.0");
        packSummaryResponse.setConsolidationAchievedQuantities(achievedResponse);

        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        doReturn(packSummaryResponse).when(spyService).calculatePacksUtilisationForConsolidation(any());
        assertThrows(RunnerException.class, () -> spyService.uploadPacking(bulkUploadRequest));
    }

    @Test
    void uploadPackingVolumeValueOverAchievedSavePackOnOverride() throws Exception{
        var spyService = Mockito.spy(packingService);
        List<Packing> packingList = List.of(testCsvPacking);
        packingList.get(0).setHazardous(false);
        packingList.get(0).setVolumeUnit(Constants.VOLUME_UNIT_M3);
        BulkUploadRequest bulkUploadRequest = new BulkUploadRequest();
        bulkUploadRequest.setConsolidationId(1L);
        bulkUploadRequest.setShipmentId(2L);
        bulkUploadRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        bulkUploadRequest.setOverride(true);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        AchievedQuantitiesResponse achievedResponse = new AchievedQuantitiesResponse();
        achievedResponse.setVolumeUtilization("100.0");
        packSummaryResponse.setConsolidationAchievedQuantities(achievedResponse);
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setTransportMode("AIR");

        when(parser.parseExcelFile(any(), any(), any(), anyMap(), any(), any(), anyMap(), anyMap(), anyMap())).thenReturn(packingList);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(new VolumeWeightChargeable());
        doReturn(packSummaryResponse).when(spyService).calculatePacksUtilisationForConsolidation(any());

        spyService.uploadPacking(bulkUploadRequest);

        verify(packingDao, times(1)).saveAll(any());
        verify(packingSync, times(1)).sync(any(), any(), any());
    }

    @Test
    void downloadPacking() {
        BulkDownloadRequest request = BulkDownloadRequest.builder().consolidationId("12").shipmentId("12").build();
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.convertToList(any(), eq(PackingExcelModel.class))).thenReturn(List.of(PackingExcelModel.builder().build()));
        Assertions.assertThrows(RunnerException.class, () -> packingService.downloadPacking(response, request));
    }

    @Test
    void downloadPacking_ConsoleIdNull() {
        BulkDownloadRequest request = BulkDownloadRequest.builder().shipmentId("12").build();
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.convertToList(any(), eq(PackingExcelModel.class))).thenReturn(List.of(PackingExcelModel.builder().build()));
        Assertions.assertThrows(RunnerException.class, () -> packingService.downloadPacking(response, request));
    }

    @Test
    void downloadPacking_ShipmentIdNull() {
        BulkDownloadRequest request = BulkDownloadRequest.builder().consolidationId("12").build();
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.convertToList(any(), eq(PackingExcelModel.class))).thenReturn(List.of(PackingExcelModel.builder().build()));
        Assertions.assertThrows(RunnerException.class, () -> packingService.downloadPacking(response, request));
    }

    @Test
    void update() throws RunnerException {
        packingRequest.setId(12L);
        testPacking.setId(12L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(packingRequest);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.update(commonRequestModel);
        assertNull(responseEntity);
    }

    @Test
    void list() {
        testPacking.setId(1L);
        ListCommonRequest getRequest = ListCommonRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Packing.class), eq(PackingResponse.class))).thenReturn((PackingResponse) packingResponse);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.list(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(packingResponse), page.getTotalPages(), page.getTotalElements()), responseEntity);
    }

    @Test
    void testList_Failure() {
        ResponseEntity<IRunnerResponse> httpResponse = packingService.list(CommonRequestModel.buildRequest());
        assertNotNull(httpResponse);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void listAsync() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel request = CommonRequestModel.buildRequest(listCommonRequest);

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = packingService.listAsync(request);

        assertNull(responseEntity);
    }

    @Test
    void delete() {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);
        ResponseEntity<IRunnerResponse> responseEntity = packingService.delete(commonRequestModel);
        assertNull(responseEntity);
    }

    @Test
    void retrieveById() {
        testPacking.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.retrieveById(commonRequestModel);

        assertNull(responseEntity);
    }

    @Test
    void calculateWeightVolumne() throws RunnerException {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(containerRequest)
                .oldPack(null)
                .newPack(packingRequest)
                .oldContainer(containerRequest).build();

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(ContainerRequest.class) , eq(Containers.class))).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(Packing.class))).thenReturn(testPacking);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = packingService.calculateWeightVolumne(CommonRequestModel.builder().data(request).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateWeightVolumne_NewPackNull() throws RunnerException {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(null)
                .oldPack(packingRequest)
                .newPack(null)
                .oldContainer(containerRequest).build();

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(ContainerRequest.class) , eq(Containers.class))).thenReturn(testContainer);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = packingService.calculateWeightVolumne(CommonRequestModel.builder().data(request).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateWeightVolumne_PacksNull() {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(null)
                .oldPack(null)
                .newPack(null)
                .oldContainer(containerRequest).build();

        mockShipmentSettings();
        when(jsonHelper.convertValue(any(ContainerRequest.class) , eq(Containers.class))).thenReturn(testContainer);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        assertThrows(NullPointerException.class, () -> packingService.calculateWeightVolumne(commonRequestModel));
    }

    @Test
    void calculateWeightVolumne_PackNull_Failure() {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(null)
                .oldPack(packingRequest)
                .newPack(null)
                .oldContainer(containerRequest).build();

        when(shipmentDao.findById(anyLong())).thenThrow(new RuntimeException());
        mockShipmentSettings();
        assertThrows(RunnerException.class, () -> packingService.calculateWeightVolumne(CommonRequestModel.builder().data(request).build()));
    }

    @Test
    void calculateWeightVolumne_NullOldAndNewCont() throws RunnerException {
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(null)
                .oldPack(null)
                .newPack(null)
                .oldContainer(null).build();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = packingService.calculateWeightVolumne(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void testCalculatePackSummary_Success() throws RunnerException {
        List<Packing> packingList = testPackingList;
        mockShipmentSettings();
        mockTenantSettings();
        PackSummaryResponse packSummaryResponse = packingService.calculatePackSummary(packingList, Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL, ShipmentMeasurementDetailsDto.builder().build());
        assertNotNull(packSummaryResponse);
        assertEquals(packSummaryResponse, jsonTestUtility.getTestPackSummaryResponse());
    }

    @Test
    void testCalculatePackSummaryDefault_Success() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).build());
        mockShipmentSettings();
        mockTenantSettings();
        PackSummaryResponse packSummaryResponse = packingService.calculatePackSummary(null, Constants.TRANSPORT_MODE_ROA, Constants.SHIPMENT_TYPE_LCL, ShipmentMeasurementDetailsDto.builder().build());
        assertNotNull(packSummaryResponse);
    }

    @Test
    void testCalculatePackSummary_AIR_Success() throws RunnerException {
        List<Packing> packingList = testPackingList;
        mockShipmentSettings();
        mockTenantSettings();
        PackSummaryResponse packSummaryResponse = packingService.calculatePackSummary(packingList, Constants.TRANSPORT_MODE_AIR, Constants.SHIPMENT_TYPE_LCL, ShipmentMeasurementDetailsDto.builder().build());
        assertNotNull(packSummaryResponse);
        assertEquals(packSummaryResponse, jsonTestUtility.getTestPackSummaryAirResponse());
    }

    @Test
    void testCalculateVolume_Success() throws RunnerException {
        packingService.calculateVolume(testAutoCalculatePackingRequest, testAutoCalculatePackingResponse);
        assertNotNull(testAutoCalculatePackingResponse);
    }

    @Test
    void testCalculateVolume_calculation() throws RunnerException {
        testAutoCalculatePackingRequest.setLength(new BigDecimal(1));
        testAutoCalculatePackingRequest.setWidth(new BigDecimal(2));
        testAutoCalculatePackingRequest.setHeight(new BigDecimal(3));
        mockShipmentSettings();
        packingService.calculateVolume(testAutoCalculatePackingRequest, testAutoCalculatePackingResponse);
        assertNotNull(testAutoCalculatePackingRequest);
    }

    @Test
    void testCalculateVolume_calculation_FT() throws RunnerException {
        testAutoCalculatePackingRequest.setLength(new BigDecimal(6));
        testAutoCalculatePackingRequest.setWidth(new BigDecimal(7));
        testAutoCalculatePackingRequest.setHeight(new BigDecimal(8));
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        mockShipmentSettings();
        packingService.calculateVolume(testAutoCalculatePackingRequest, testAutoCalculatePackingResponse);
        assertNotNull(testAutoCalculatePackingRequest);
    }

    @Test
    void testCalculateVolume_calculation_IN() throws RunnerException {
        testAutoCalculatePackingRequest.setLength(new BigDecimal(1));
        testAutoCalculatePackingRequest.setWidth(new BigDecimal(2));
        testAutoCalculatePackingRequest.setHeight(new BigDecimal(3));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setVolumeDecimalPlace(3);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        packingService.calculateVolume(testAutoCalculatePackingRequest, testAutoCalculatePackingResponse);
        assertNotNull(testAutoCalculatePackingRequest);
    }

    @Test
    void testAutoCalculateVolumetricWeight_Success() {
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        ResponseEntity<IRunnerResponse> responseEntity = packingService.autoCalculatePacksData(commonRequest);
        assertNotNull(responseEntity);
    }

    @Test
    void testAutoCalculateVolumetricWeight_calculationAir() throws Exception{
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getVolumeWeightChargeable());
        ResponseEntity<IRunnerResponse> responseEntity = packingService.autoCalculatePacksData(commonRequest);
        assertNotNull(responseEntity);
    }

    @Test
    void testAutoCalculateVolumetricWeight_calculationSea() throws Exception{
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testAutoCalculatePackingRequest.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getVolumeWeightChargeable());
        ResponseEntity<IRunnerResponse> responseEntity = packingService.autoCalculatePacksData(commonRequest);
        assertNotNull(responseEntity);
    }

    @Test
    void testListPacksToDetach_Success() throws RunnerException {
        DetachPacksListDto request = DetachPacksListDto.builder().containerId(1L).pageSize(1).shipmentId(1L).pageNo(1).build();
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = packingService.listPacksToDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void testV1PackingCreateAndUpdate_Success() throws RunnerException {
        when(packingDao.findByGuid(any())).thenReturn(Optional.of(testPacking));
        when(modelMapper.map(any(), any())).thenReturn(testPacking);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), false);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1PackingCreateAndUpdate_Success_SyncQueue() throws RunnerException, NoSuchFieldException, IllegalAccessException {
        Field field = SyncConfig.class.getField("IS_REVERSE_SYNC_ACTIVE");
        field.setAccessible(true);
        field.set(syncConfig, false);
        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), true);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1PackingCreateAndUpdate_NewPack_Success() throws RunnerException {
        when(packingDao.findByGuid(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), any())).thenReturn(testPacking);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDao.findByGuid(any())).thenReturn(Optional.of(testConsolidation));
        when(packingDao.save(any())).thenReturn(testPacking);
        when(objectMapper.convertValue(any(), eq(PackingResponse.class))).thenReturn(packingResponse);
        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), false);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1PackingCreateAndUpdate_Failure() {
        when(packingDao.findByGuid(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), any())).thenReturn(testPacking);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDao.findByGuid(any())).thenReturn(Optional.of(testConsolidation));
        when(packingDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(testPackingRequestV2);
        var e  = assertThrows(RuntimeException.class, () -> packingService.V1PackingCreateAndUpdate(commonRequestModel, false));
        assertNotNull(e);
    }

    @Test
    void testV1BulkPackingCreateAndUpdate() {
        BulkPackingRequestV2 bulkPackingRequestV2 = BulkPackingRequestV2.builder()
                .bulkPacking(List.of(testPackingRequestV2))
                .ConsolidationId(1L)
                .ShipmentId(1L)
                .build();
        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1BulkPackingCreateAndUpdate(CommonRequestModel.buildRequest(bulkPackingRequestV2));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1BulkPackingCreateAndUpdate_Failure() throws RunnerException{
        BulkPackingRequestV2 bulkPackingRequestV2 = BulkPackingRequestV2.builder()
                .bulkPacking(List.of(testPackingRequestV2))
                .ConsolidationId(1L)
                .ShipmentId(1L)
                .build();
        PackingService spyService = spy(packingService);
        doThrow(new RuntimeException()).when(spyService).V1PackingCreateAndUpdate(any(), anyBoolean());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(bulkPackingRequestV2);
        var e = assertThrows(RuntimeException.class, () -> spyService.V1BulkPackingCreateAndUpdate(commonRequestModel));
        assertNotNull(e);
    }

    @Test
    void testCalculatePacksUtilisationForConsolidationPacks() throws RunnerException{
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setConsolidationId(consolidationId);
        request.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        AchievedQuantitiesResponse achievedQuantitiesResponse = objectMapperTest.convertValue(achievedQuantities, AchievedQuantitiesResponse.class);

        // Mocking
        mockShipmentSettings();
        mockTenantSettings();
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValue(any(), eq(Allocations.class))).thenReturn(allocations);
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.calculateConsolUtilization(consolidationDetails)).thenReturn(consolidationDetails.setAchievedQuantities(achievedQuantities));
        when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(achievedQuantitiesResponse);

        try{
            var responseEntity = packingService.calculatePacksUtilisationForConsolidation(request);
            assertNotNull(responseEntity);
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void testCalculatePacksUtilisationForConsolidationPacksForIneligibleConsolidation() throws RunnerException{
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);

        //Invalid consol conditions
        consolidationDetails.setTransportMode("SEA");
        consolidationDetails.setShipmentType("IMP");
        consolidationDetails.setAllocations(null);


        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setConsolidationId(consolidationId);
        request.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        objectMapperTest.convertValue(achievedQuantities, AchievedQuantitiesResponse.class);
        var packingServiceSpy = Mockito.spy(packingService);

        // Mocking
        mockTenantSettings();
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValue(any(), eq(Allocations.class))).thenReturn(null);
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        doReturn(new PackSummaryResponse()).when(packingServiceSpy).calculatePackSummary(anyList(), any(), any(), any());

        try{
            var packSummaryResponse = packingServiceSpy.calculatePacksUtilisationForConsolidation(request);
            assertNull(packSummaryResponse.getAchievedVolume());
            assertNull(packSummaryResponse.getAchievedWeight());
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void testCalculatePacksUtilisationForAttachedShipmentPacks() throws RunnerException{
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(packingList);
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).build();
        shipmentRequest.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setShipmentRequest(shipmentRequest);
        request.setConsolidationId(consolidationId);

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        AchievedQuantitiesResponse achievedQuantitiesResponse = objectMapperTest.convertValue(achievedQuantities, AchievedQuantitiesResponse.class);

        // Mocking
        mockShipmentSettings();
        mockTenantSettings();
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValue(any(), eq(Allocations.class))).thenReturn(allocations);
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.calculateConsolUtilization(consolidationDetails)).thenReturn(consolidationDetails.setAchievedQuantities(achievedQuantities));
        when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(achievedQuantitiesResponse);

        try{
            var responseEntity = packingService.calculatePacksUtilisationForConsolidation(request);
            assertNotNull(responseEntity);
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void testCalculatePacksUtilisationForAttachedShipmentEmptyRequest() throws RunnerException{
        List<Packing> packingList = Collections.emptyList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(packingList);
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().build();
        shipmentRequest.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setShipmentRequest(shipmentRequest);
        request.setConsolidationId(consolidationId);

        // Mocking
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValue(any(), eq(Allocations.class))).thenReturn(allocations);
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));

        try{
            var responseEntity = packingService.calculatePacksUtilisationForConsolidation(request);
            assertNull(responseEntity);
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void testCalculatePacksUtilisationForAttachingShipments() throws RunnerException{
        var packingList = jsonTestUtility.getTestPackingList();

        Long consolidationId = 1L;
        List<Long> shipmentIds = List.of(1L);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        consolidationDetails.setPackingList(packingList);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setShipmentIdList(shipmentIds);
        request.setConsolidationId(consolidationId);

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        AchievedQuantitiesResponse achievedQuantitiesResponse = objectMapperTest.convertValue(achievedQuantities, AchievedQuantitiesResponse.class);

        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(List.of(ShipmentDetails.builder()
            .packingList(packingList).build()
        ));

        // Mocking
        mockShipmentSettings();
        mockTenantSettings();
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValue(any(), eq(Allocations.class))).thenReturn(allocations);
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.calculateConsolUtilization(consolidationDetails)).thenReturn(consolidationDetails.setAchievedQuantities(achievedQuantities));
        when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(achievedQuantitiesResponse);
        when(shipmentDao.findAll(any(), any())).thenReturn(shipmentDetailsPage);

        try{
            var responseEntity = packingService.calculatePacksUtilisationForConsolidation(request);
            assertNotNull(responseEntity);
            verify(commonUtils, times(0)).setInterBranchContextForHub();
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void savePackUtilisationCalculationInConsoleSuccess() throws RunnerException {
        var spyService = Mockito.spy(packingService);
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(packingList);
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).build();
        shipmentRequest.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setSaveConsol(true);
        request.setShipmentRequest(shipmentRequest);
        request.setConsolidationId(consolidationId);

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        AchievedQuantitiesResponse achievedQuantitiesResponse = objectMapperTest.convertValue(achievedQuantities, AchievedQuantitiesResponse.class);

        // Mocking
        mockShipmentSettings();
        mockTenantSettings();
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValue(any(), eq(Allocations.class))).thenReturn(allocations);
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.calculateConsolUtilization(consolidationDetails)).thenReturn(consolidationDetails.setAchievedQuantities(achievedQuantities));
        when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(achievedQuantitiesResponse);

        try{
            spyService.savePackUtilisationCalculationInConsole(request);
            verify(consolidationDao, times(1)).save(any(), anyBoolean());
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void savePackUtilisationCalculationInConsoleConsolidationNotPresent() {
        var spyService = Mockito.spy(packingService);
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(packingList);
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).build();
        shipmentRequest.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setSaveConsol(true);
        request.setShipmentRequest(shipmentRequest);
        request.setConsolidationId(consolidationId);

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        // Mocking
        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.empty());

        try{
            spyService.savePackUtilisationCalculationInConsole(request);
            verify(consolidationDao, times(0)).save(any(), anyBoolean());
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void savePackUtilisationCalculationInConsoleFails() {
        var spyService = Mockito.spy(packingService);
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(packingList);
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).build();
        shipmentRequest.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setSaveConsol(true);
        request.setShipmentRequest(shipmentRequest);
        request.setConsolidationId(consolidationId);

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        // Mocking
        when(consolidationDao.findById(consolidationId)).thenThrow(new RuntimeException());

        try{
            spyService.savePackUtilisationCalculationInConsole(request);
            verify(consolidationDao, times(0)).save(any(), anyBoolean());
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void savePackUtilisationCalculationSkipsInCaseOfNonAirTransportMode() {
        var spyService = Mockito.spy(packingService);
        var packingList = jsonTestUtility.getTestPackingList();
        Long consolidationId = 1L;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(packingList);
        consolidationDetails.setTransportMode("SEA");
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).build();
        shipmentRequest.setPackingList(jsonTestUtility.convertValueToList(packingList, PackingRequest.class));
        Allocations allocations = jsonTestUtility.getJson("CONSOLIDATION_ALLOCATION", Allocations.class);
        consolidationDetails.setAllocations(allocations);
        setFeatureFlagForInterBranch();

        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setSaveConsol(true);
        request.setShipmentRequest(shipmentRequest);
        request.setConsolidationId(consolidationId);

        AchievedQuantities achievedQuantities = new AchievedQuantities();
        BigDecimal consolidatedWeight = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getWeight().doubleValue()).sum());
        BigDecimal consolidatedVolume = BigDecimal.valueOf(packingList.stream().mapToDouble(i -> i.getVolume().doubleValue()).sum());
        achievedQuantities.setConsolidatedWeight(consolidatedWeight);
        achievedQuantities.setConsolidatedVolume(consolidatedVolume);
        achievedQuantities.setWeightUtilization(String.valueOf(consolidatedWeight.divide(allocations.getWeight(), RoundingMode.HALF_UP)));
        achievedQuantities.setVolumeUtilization(String.valueOf(consolidatedVolume.divide(allocations.getVolume(), RoundingMode.HALF_UP)));

        // Mocking
        when(consolidationDao.findById(consolidationId)).thenThrow(new RuntimeException());

        try{
            spyService.savePackUtilisationCalculationInConsole(request);
            verify(consolidationDao, times(0)).save(any(), anyBoolean());
        }
        catch (Exception e) {
            fail(e);
        }
    }



    void setFeatureFlagForInterBranch() {
        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);
    }



}
