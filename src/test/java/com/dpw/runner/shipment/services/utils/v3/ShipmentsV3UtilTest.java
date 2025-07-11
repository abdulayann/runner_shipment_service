package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
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
import com.dpw.runner.shipment.services.utils.*;
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
import java.util.concurrent.Executors;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;


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

}
