package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ContainerResult;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.ProductIdentifierUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentsV3UtilTest extends CommonMocks {

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private GetNextNumberHelper getNextNumberHelper;
    @Mock
    private IV1Service v1Service;
    @Mock
    private ProductIdentifierUtility productEngine;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IDateTimeChangeLogService dateTimeChangeLogService;
    @InjectMocks
    private CommonUtils commonUtils;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ConsolidationDao consolidationDetailsDao;
    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private IAwbDao awbDao;
    @Mock
    private IEventDao eventDao;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private INotesDao notesDao;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IServiceDetailsDao serviceDetailsDao;
    @Mock
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    @Mock
    private EventsV3Util eventsV3Util;
    @Mock
    private IEventsV3Service eventsV3Service;
    @Mock
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @InjectMocks
    private ShipmentsV3Util shipmentsV3Util;

    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapper;

    @BeforeEach
    void setup() throws IOException {
        MockitoAnnotations.openMocks(this);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(true).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        shipmentsV3Util.executorService = Executors.newFixedThreadPool(2);
        JsonTestUtility jsonTestUtility = new JsonTestUtility();
        testShipment = jsonTestUtility.getTestShipment();
        objectMapper = JsonTestUtility.getMapper();
    }

    @AfterEach
    void tearDown() {
        shipmentsV3Util.executorService.shutdown();
    }


    @Test
    void testGenerateShipmentId_CustomSequence_Success() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        // Page with zero results to simulate "shipment ID is unique"
        Page<ShipmentDetails> page = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        when(shipmentSettingsDao.findByTenantId(anyInt()))
                .thenReturn(Optional.of(settings));

        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of());
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn("CUST-SHIP-001");

        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(page);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("CUST-SHIP-001", shipmentId);
    }

    @Test
    void testGenerateShipmentId_CustomSequenceFails_FallbackToDefault() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);
        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));

        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of());
        when(productEngine.getCommonSequenceNumber(any(), any())).thenThrow(new RuntimeException("Error"));

        when(v1Service.getShipmentSerialNumber()).thenReturn("FALLBACK-001");

        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000FALLBACK-001", shipmentId); // Assuming fallback uses this prefix
    }


    @Test
    void testGenerateShipmentId_NoShipmentSettings() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("SEA");

        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.empty());
        when(v1Service.getShipmentSerialNumber()).thenReturn("DEFAULT-123");

        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000DEFAULT-123", shipmentId);
    }

    @Test
    void testGenerateShipmentId_ShipmentIdAlreadyExists_RetryUntilUnique() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        // First response: "DUPLICATE-ID" already exists
        Page<ShipmentDetails> nonEmptyPage = new PageImpl<>(List.of(new ShipmentDetails()));
        // Second response: "UNIQUE-ID" does not exist
        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);
        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));

        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of());

        when(productEngine.getCommonSequenceNumber(any(), any()))
                .thenReturn("DUPLICATE-ID")
                .thenReturn("UNIQUE-ID");

        // Simulate retry: first returns existing shipment, second returns empty
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(nonEmptyPage)
                .thenReturn(nonEmptyPage)
                .thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("UNIQUE-ID", shipmentId);
    }

    @Test
    void testGenerateShipmentId_CustomSequenceIsEmpty_FallsBackToSerial() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of());
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn(""); // empty sequence
        when(v1Service.getShipmentSerialNumber()).thenReturn("SERIAL-001");
        when(shipmentDao.findAll(any(), any())).thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000SERIAL-001", shipmentId);
    }

    @Test
    void testGenerateShipmentId_IdentifiedProductNull_FallsBack() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of());
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn(null);
        when(productEngine.identifyProduct(any(ShipmentDetails.class), any())).thenReturn(null); // product is null

        when(v1Service.getShipmentSerialNumber()).thenReturn("SERIAL-002");
        when(shipmentDao.findAll(any(), any())).thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000SERIAL-002", shipmentId);
    }

    @Test
    void testGenerateShipmentId_SequenceSettingsNull_DefaultProductAlsoNull() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of());
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn(null);

        var dummyProduct = mock(TenantProducts.class);
        when(productEngine.identifyProduct(any(ShipmentDetails.class), any())).thenReturn(dummyProduct);
        when(getNextNumberHelper.getProductSequence(any(), any())).thenReturn(null);
        when(productEngine.getShipmentProductWithOutContainerType(any(), any(), any())).thenReturn(null);
        when(productEngine.getDefaultShipmentProduct(any())).thenReturn(null); // even default is null

        when(v1Service.getShipmentSerialNumber()).thenReturn("SERIAL-003");
        when(shipmentDao.findAll(any(), any())).thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000SERIAL-003", shipmentId);
    }

    @Test
    void testGenerateShipmentId_CustomizedSequenceDisabled_Fallback() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        Page<ShipmentDetails> emptyPage = new PageImpl<>(List.of());

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(false); // customized flag off

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));
        when(v1Service.getShipmentSerialNumber()).thenReturn("SERIAL-004");
        when(shipmentDao.findAll(any(), any())).thenReturn(emptyPage);

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000SERIAL-004", shipmentId);
    }

    @Test
    void testGenerateShipmentId_FallbackToShipmentProductWithoutContainerType_Success() throws RunnerException {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        ProductSequenceConfig fallbackSequence = new ProductSequenceConfig();
        fallbackSequence.setPrefix("SHPC-");

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));

        TenantProducts identifiedProduct = new TenantProducts();
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(identifiedProduct));
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn(""); // returns empty string (not null)
        when(productEngine.identifyProduct(any(ShipmentDetails.class), any())).thenReturn(identifiedProduct);
        when(getNextNumberHelper.getProductSequence(any(), any())).thenReturn(null);
        when(productEngine.getShipmentProductWithOutContainerType(any(), any(), any())).thenReturn(fallbackSequence);
        when(getNextNumberHelper.generateCustomSequence(
                eq(fallbackSequence), eq("SHPC-"), anyInt(), eq(true), isNull(), eq(false)
        )).thenReturn("SHPC-456");

        when(shipmentDao.findAll(any(), any()))
                .thenReturn(new PageImpl<>(List.of())); // unique

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHPC-456", shipmentId);
    }


    @Test
    void testGenerateShipmentId_FallbackToDefaultProductSequence_Success() throws RunnerException {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        ProductSequenceConfig defaultSequence = new ProductSequenceConfig();
        defaultSequence.setPrefix("DFT-");

        TenantProducts identifiedProduct = new TenantProducts();
        identifiedProduct.setId(1L);

        TenantProducts defaultProduct = new TenantProducts();
        defaultProduct.setId(2L); // ensure not same as identified

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(identifiedProduct, defaultProduct));
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn(""); // force fallback
        when(productEngine.identifyProduct(any(ShipmentDetails.class), any())).thenReturn(identifiedProduct);
        when(getNextNumberHelper.getProductSequence(eq(1L), any())).thenReturn(null); // first fails
        when(productEngine.getShipmentProductWithOutContainerType(any(), any(), any())).thenReturn(null); // second fails
        when(productEngine.getDefaultShipmentProduct(any())).thenReturn(defaultProduct);
        when(getNextNumberHelper.getProductSequence(eq(2L), any())).thenReturn(defaultSequence);
        when(getNextNumberHelper.generateCustomSequence(
                eq(defaultSequence), eq("DFT-"), anyInt(), eq(true), isNull(), eq(false)
        )).thenReturn("DFT-999");

        when(shipmentDao.findAll(any(), any()))
                .thenReturn(new PageImpl<>(List.of())); // shipment ID is unique

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("DFT-999", shipmentId);
    }

    @Test
    void testGenerateShipmentId_DefaultProductSameAsIdentified_ReturnsEmptyAndFallbacks() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCustomisedSequence(true);

        TenantProducts identifiedProduct = new TenantProducts();
        identifiedProduct.setId(1L);

        when(shipmentSettingsDao.findByTenantId(anyInt())).thenReturn(Optional.of(settings));
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(identifiedProduct));
        when(productEngine.getCommonSequenceNumber(any(), any())).thenReturn("");
        when(productEngine.identifyProduct(any(ShipmentDetails.class), any())).thenReturn(identifiedProduct);
        when(getNextNumberHelper.getProductSequence(any(), any())).thenReturn(null);
        when(productEngine.getShipmentProductWithOutContainerType(any(), any(), any())).thenReturn(null);
        when(productEngine.getDefaultShipmentProduct(any())).thenReturn(identifiedProduct); // same as identified → fallback "" path

        when(v1Service.getShipmentSerialNumber()).thenReturn("SERIAL-777");

        // Simulate retry until we get unique ID
        when(shipmentDao.findAll(any(), any()))
                .thenReturn(new PageImpl<>(List.of())); // 1st loop run - final shipment ID check

        String shipmentId = shipmentsV3Util.generateShipmentId(shipmentDetails);

        assertEquals("SHP000SERIAL-777", shipmentId); // Final fallback
    }

    @Test
    void testAfterSaveforEt() {
        ShipmentDetails shipmentDetails = testShipment;
        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentEtV3Request.class);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockTenantSettings();
        List<Long> removedConsolIds = new ArrayList<>();
        MutableBoolean isNewConsolAttached = new MutableBoolean(false);
        boolean includeGuid = true;
        assertDoesNotThrow(() -> shipmentsV3Util.afterSaveforEt(shipmentDetails, null, true, mockShipmentRequest, shipmentSettingsDetails, includeGuid));

    }

    @Test
    void testAfterSaveforEt2() {
        ShipmentDetails shipmentDetails = testShipment;
        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentEtV3Request.class);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(true).build());
        mockTenantSettings();
        List<Long> removedConsolIds = new ArrayList<>();
        MutableBoolean isNewConsolAttached = new MutableBoolean(false);
        boolean includeGuid = true;
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        assertDoesNotThrow(() -> shipmentsV3Util.afterSaveforEt(shipmentDetails, null, true, mockShipmentRequest, shipmentSettingsDetails, includeGuid));

    }

    @Test
    void testAfterSaveforEt3() {
        ShipmentDetails shipmentDetails = testShipment;
        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentEtV3Request.class);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().build());
        mockTenantSettings();
        List<Long> removedConsolIds = new ArrayList<>();
        MutableBoolean isNewConsolAttached = new MutableBoolean(false);
        boolean includeGuid = true;
        assertDoesNotThrow(() -> shipmentsV3Util.afterSaveforEt(shipmentDetails, null, true, mockShipmentRequest, shipmentSettingsDetails, includeGuid));

    }

    @Test
    void testGenerateSummaryResponseWarnings_NoWarnings() throws RunnerException {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setWeightUnit("KG");
        shipment.setVolumeUnit("M3");

        CargoDetailsResponse packsSummary = CargoDetailsResponse.builder()
                .noOfPacks(10)
                .packsUnit("PKG")
                .weight(BigDecimal.valueOf(100))
                .weightUnit("KG")
                .volume(BigDecimal.valueOf(20))
                .volumeUnit("M3")
                .build();

        CargoDetailsResponse containerSummary = CargoDetailsResponse.builder()
                .noOfPacks(10)
                .packsUnit("PKG")
                .weight(BigDecimal.valueOf(100))
                .weightUnit("KG")
                .volume(BigDecimal.valueOf(20))
                .volumeUnit("M3")
                .build();

        ShipmentSummaryWarningsResponse warnings =
                shipmentsV3Util.generateSummaryResponseWarnings(shipment, packsSummary, containerSummary);

        assertNotNull(warnings);
        assertNull(warnings.getPackagesWarning());
        assertNull(warnings.getWeightWarning());
        assertNull(warnings.getVolumeWarning());
    }

    @Test
    void testGenerateSummaryResponseWarnings_PackagesMismatch() throws RunnerException {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setWeightUnit("KG");
        shipment.setVolumeUnit("M3");

        CargoDetailsResponse packsSummary = CargoDetailsResponse.builder()
                .noOfPacks(8)
                .packsUnit("PKG")
                .weight(BigDecimal.valueOf(100))
                .weightUnit("KG")
                .volume(BigDecimal.valueOf(20))
                .volumeUnit("M3")
                .build();

        CargoDetailsResponse containerSummary = CargoDetailsResponse.builder()
                .noOfPacks(10)
                .packsUnit("PKG")
                .weight(BigDecimal.valueOf(100))
                .weightUnit("KG")
                .volume(BigDecimal.valueOf(20))
                .volumeUnit("M3")
                .build();

        ShipmentSummaryWarningsResponse warnings =
                shipmentsV3Util.generateSummaryResponseWarnings(shipment, packsSummary, containerSummary);

        assertNotNull(warnings);
        assertNotNull(warnings.getPackagesWarning());
        assertEquals("10 PKG", warnings.getPackagesWarning().getContainerValue());
        assertEquals("8 PKG", warnings.getPackagesWarning().getPackageValue());
        assertEquals("2 PKG", warnings.getPackagesWarning().getDifference());

        // Weight and volume warnings should be null since values match
        assertNull(warnings.getWeightWarning());
        assertNull(warnings.getVolumeWarning());
    }

    @Test
    void testGenerateSummaryResponseWarnings_WeightAndVolumeWarnings() throws RunnerException {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setWeightUnit("KG");
        shipment.setVolumeUnit("M3");

        CargoDetailsResponse packsSummary = CargoDetailsResponse.builder()
                .noOfPacks(10)
                .packsUnit("PKG")
                .weight(BigDecimal.valueOf(150))
                .weightUnit("KG")
                .volume(BigDecimal.valueOf(25))
                .volumeUnit("M3")
                .build();

        CargoDetailsResponse containerSummary = CargoDetailsResponse.builder()
                .noOfPacks(10)
                .packsUnit("PKG")
                .weight(BigDecimal.valueOf(100))
                .weightUnit("KG")
                .volume(BigDecimal.valueOf(20))
                .volumeUnit("M3")
                .build();

        ShipmentSummaryWarningsResponse warnings =
                shipmentsV3Util.generateSummaryResponseWarnings(shipment, packsSummary, containerSummary);

        assertNotNull(warnings);
        assertNull(warnings.getPackagesWarning());

        assertNotNull(warnings.getWeightWarning());
        assertTrue(warnings.getWeightWarning().getShowWarning());
        assertEquals("100 KG", warnings.getWeightWarning().getContainerValue());
        assertEquals("150 KG", warnings.getWeightWarning().getPackageValue());
        assertEquals("50 KG", warnings.getWeightWarning().getDifference());

        assertNotNull(warnings.getVolumeWarning());
        assertTrue(warnings.getVolumeWarning().getShowWarning());
        assertEquals("20 M3", warnings.getVolumeWarning().getContainerValue());
        assertEquals("25 M3", warnings.getVolumeWarning().getPackageValue());
        assertEquals("5 M3", warnings.getVolumeWarning().getDifference());
    }

    @Test
    void testResolveUnit_AllSameUnits_ReturnsThatUnit() {
        List<String> units = List.of("KG", "KG", "KG");
        String branchDefaultUnit = "LB";

        String result = shipmentsV3Util.resolveUnit(units, branchDefaultUnit);

        assertEquals("KG", result);
    }

    @Test
    void testResolveUnit_MultipleDifferentUnits_ReturnsBranchDefault() {
        List<String> units = List.of("KG", "LB", "KG");
        String branchDefaultUnit = "LB";

        String result = shipmentsV3Util.resolveUnit(units, branchDefaultUnit);

        assertEquals("LB", result);
    }

    @Test
    void testResolveUnit_NullValuesFilteredOut() {
        List<String> units = List.of("KG", "KG");
        String branchDefaultUnit = "LB";

        String result = shipmentsV3Util.resolveUnit(units, branchDefaultUnit);

        assertEquals("KG", result);
    }

    @Test
    void testResolveUnit_EmptyList_ReturnsBranchDefault() {
        List<String> units = Collections.emptyList();
        String branchDefaultUnit = "LB";

        String result = shipmentsV3Util.resolveUnit(units, branchDefaultUnit);

        assertEquals("LB", result);
    }

    @Test
    void testBuildShipmentResponse_AllValuesSet_WithNonZeroPacks() {
        VolumeWeightChargeable vw = new VolumeWeightChargeable();
        vw.setChargeable(BigDecimal.valueOf(100));
        vw.setChargeableUnit("KG");
        vw.setVolumeWeight(BigDecimal.valueOf(200));
        vw.setVolumeWeightUnit("M3");

        ContainerResult containerResult = new ContainerResult(5, 1, BigDecimal.valueOf(3));

        CargoDetailsResponse response = shipmentsV3Util.buildShipmentResponse(
                10, 2, "PKG", "DG-PKG",
                vw, containerResult,
                BigDecimal.valueOf(1000), "KG",
                BigDecimal.valueOf(20), "M3"
        );

        assertNotNull(response);
        assertEquals(10, response.getNoOfPacks());
        assertEquals("PKG", response.getPacksUnit());
        assertEquals(2, response.getDgPacks());
        assertEquals("DG-PKG", response.getDgPacksUnit());
        assertEquals(5, response.getContainers());
        assertEquals(BigDecimal.valueOf(3), response.getTeuCount());
        assertEquals(BigDecimal.valueOf(100), response.getChargable());
        assertEquals("KG", response.getChargeableUnit());
        assertEquals(BigDecimal.valueOf(200), response.getVolumetricWeight());
        assertEquals("M3", response.getVolumetricWeightUnit());
    }

    @Test
    void testBuildShipmentResponse_PacksZero_SetsNoOfPacksNull() {
        VolumeWeightChargeable vw = new VolumeWeightChargeable();

        ContainerResult containerResult = new ContainerResult(5, 0, BigDecimal.ZERO);

        CargoDetailsResponse response = shipmentsV3Util.buildShipmentResponse(
                0, null, "PKG", "DG-PKG",
                vw, containerResult,
                BigDecimal.ZERO, "KG",
                BigDecimal.ZERO, "M3"
        );

        assertNull(response.getNoOfPacks());
    }

    @Test
    void testGenerateWarning_NoMismatch_ReturnsNull() throws RunnerException {
        BigDecimal val = BigDecimal.valueOf(100);
        String unit = "KG";
        String shipmentUnit = "KG";

        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                val, unit,
                val, unit,
                Constants.MASS,
                shipmentUnit);

        assertNull(warning);
    }

    @Test
    void testGenerateWarning_Mismatch_ReturnsWarningDetail() throws RunnerException {
        BigDecimal packVal = BigDecimal.valueOf(150);
        BigDecimal contVal = BigDecimal.valueOf(100);
        String packUnit = "KG";
        String contUnit = "KG";
        String shipmentUnit = "KG";

        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                packVal, packUnit,
                contVal, contUnit,
                Constants.MASS,
                shipmentUnit);

        assertNotNull(warning);
        assertTrue(warning.getShowWarning());

        assertEquals("100 KG", warning.getContainerValue());
        assertEquals("150 KG", warning.getPackageValue());
        assertEquals("50 " + shipmentUnit, warning.getDifference());
    }

    @Test
    void testGetPacksType_ListWithMixedHazardous() {
        Packing safePacking = new Packing();
        safePacking.setPacksType("PKG1");
        safePacking.setHazardous(false);

        Packing dangerousPacking = new Packing();
        dangerousPacking.setPacksType("PKG2");
        dangerousPacking.setHazardous(true);

        List<Packing> packingList = Arrays.asList(safePacking, dangerousPacking);

        // Both units distinct → fallback to branch default "PKG" expected for both (due to distinct units)
        // But here we have 2 different units → will fallback to PackingConstants.PKG for both i.e. "PKG"
        List<String> result = shipmentsV3Util.getPacksType(packingList);

        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("PKG", result.get(0)); // packs unit resolved
        assertEquals("PKG2", result.get(1)); // dg packs unit resolved
    }

    @Test
    void testGetPacksType_SetContainersWithHazardous() {
        Containers safeContainer = new Containers();
        safeContainer.setId(1L);
        safeContainer.setPacksType("PKG1");
        safeContainer.setHazardous(false);

        Containers dgContainer = new Containers();
        dgContainer.setId(2L);
        dgContainer.setPacksType("PKG2");
        dgContainer.setHazardous(true);

        Set<Containers> containersSet = new HashSet<>(Arrays.asList(safeContainer, dgContainer));

        List<String> result = shipmentsV3Util.getPacksType(containersSet);

        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("PKG", result.get(0)); // packs unit resolved
        assertEquals("PKG2", result.get(1)); // dg packs unit resolved
    }

    @Test
    void testGetContainerResult_NullOrEmptySet_ReturnsEmptyResult() {
        ContainerResult result = shipmentsV3Util.getContainerResult(null);
        assertNotNull(result);
        assertEquals(0, result.getContainerCount());

        result = shipmentsV3Util.getContainerResult(Collections.emptySet());
        assertNotNull(result);
        assertEquals(0, result.getContainerCount());
    }

    @Test
    void testGetContainerResult_WithContainers() {
        Containers container1 = new Containers();
        container1.setId(1L);
        container1.setContainerCount(2L);
        container1.setTeu(BigDecimal.valueOf(2));
        container1.setHazardous(true);

        Containers container2 = new Containers();
        container2.setId(2L);
        container2.setContainerCount(3L);
        container2.setTeu(BigDecimal.valueOf(1));
        container2.setHazardous(false);

        Set<Containers> containersSet = Set.of(container1, container2);

        ContainerResult result = shipmentsV3Util.getContainerResult(containersSet);

        assertEquals(5, result.getContainerCount());
        assertEquals(1, result.getDgContCount());
        assertEquals(BigDecimal.valueOf(3), result.getTeuCount());
    }

    @Test
    void testCalculateWeightFromContainersFallBack() throws RunnerException {
        Containers container1 = new Containers();
        container1.setId(1L);
        container1.setContainerCount(2L);
        container1.setHazardous(true);
        container1.setTeu(BigDecimal.valueOf(2));
        container1.setGrossWeight(BigDecimal.valueOf(100));
        container1.setGrossWeightUnit("KG");

        Containers container2 = new Containers();
        container2.setId(2L);
        container2.setContainerCount(3L);
        container2.setHazardous(false);
        container2.setTeu(BigDecimal.valueOf(1));
        container2.setGrossWeight(BigDecimal.valueOf(50));
        container2.setGrossWeightUnit("KG");

        Set<Containers> containers = Set.of(container1, container2);

        ContainerResult result = shipmentsV3Util.calculateWeightFromContainersFallBack(containers, "KG");

        assertEquals(5, result.getContainerCount());
        assertEquals(2, result.getDgContCount());
        assertEquals(BigDecimal.valueOf(3), result.getTeuCount());
        assertEquals(BigDecimal.valueOf(150.0), result.getTotalWeight());
    }

    @Test
    void testGenerateWarning_nullPackValAndContVal_returnsNull() throws RunnerException {
        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                null, "KG", null, "KG", Constants.MASS, "KG");
        assertNull(warning);
    }

    @Test
    void testGenerateWarning_nullPackVal_returnsNull() throws RunnerException {
        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                null, "KG", BigDecimal.TEN, "KG", Constants.MASS, "KG");
        assertNull(warning);
    }

    @Test
    void testGenerateWarning_nullContVal_returnsNull() throws RunnerException {
        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                BigDecimal.TEN, "KG", null, "KG", Constants.MASS, "KG");
        assertNull(warning);
    }

    @Test
    void testGenerateWarning_equalValuesAfterConversion_returnsNull() throws RunnerException {
        BigDecimal value = BigDecimal.valueOf(10);

        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                value, "KG",
                value, "KG",
                Constants.MASS,
                "KG");

        assertNull(warning);
    }

    @Test
    void testGenerateWarning_unequalValuesAfterConversion_returnsWarning() throws RunnerException {
        BigDecimal packVal = BigDecimal.valueOf(15);
        BigDecimal contVal = BigDecimal.valueOf(10);

        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                packVal, "KG",
                contVal, "KG",
                Constants.MASS,
                "KG");

        assertNotNull(warning);
        assertTrue(warning.getShowWarning());
        assertEquals("10 KG", warning.getContainerValue());
        assertEquals("15 KG", warning.getPackageValue());
        assertEquals("5 KG", warning.getDifference());
    }

    // Additional test for stripTrailingZeros with decimal values and units
    @Test
    void testGenerateWarning_decimalValues_properFormatting() throws RunnerException {
        BigDecimal packVal = new BigDecimal("15.0000");
        BigDecimal contVal = new BigDecimal("10.5000");

        ShipmentSummaryWarningsResponse.WarningDetail warning = shipmentsV3Util.generateWarning(
                packVal, "KG",
                contVal, "KG",
                Constants.MASS,
                "KG");

        assertNotNull(warning);
        assertEquals("10.5 KG", warning.getContainerValue());
        assertEquals("15 KG", warning.getPackageValue());
        assertEquals("4.5 KG", warning.getDifference());
    }

}
