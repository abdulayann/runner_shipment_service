package com.dpw.runner.shipment.services.syncing.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.PageImpl;


import java.util.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SyncEntityConversionServiceTest {
    @Mock
    private IAuditLogDao iAuditLogDao;

    @Mock
    private IConsolidationDetailsDao iConsolidationDetailsDao;

    @Mock
    private IShipmentDao iShipmentDao;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private ObjectMapper objectMapper;

    @InjectMocks
    private SyncEntityConversionService syncEntityConversionService;

    @BeforeEach
    void setUp() {
        SyncingContext.setContext(Boolean.TRUE);
    }

    /**
     * Method under test:
     * {@link SyncEntityConversionService#packingsV2ToV1(List, List, UUID, UUID)}
     */
    @Test
    void testPackingsV2ToV1() {
        // Arrange
        ArrayList<Packing> packingList = new ArrayList<>();
        ArrayList<Containers> containers = new ArrayList<>();
        UUID shipmentGuid = UUID.randomUUID();

        // Act and Assert
        assertTrue(
                syncEntityConversionService.packingsV2ToV1(packingList, containers, shipmentGuid, UUID.randomUUID()).isEmpty());
    }

    /**
     * Method under test:
     * {@link SyncEntityConversionService#packingsV2ToV1(List, List, UUID, UUID)}
     */
    @Test
    void testPackingsV2ToV12() {
        // Arrange
        PackingRequestV2 packingRequestV2 = new PackingRequestV2();
        packingRequestV2.setConsolidationGuid(UUID.randomUUID());
        packingRequestV2.setConsolidationId(1L);
        packingRequestV2.setContainerNumber("42");
        packingRequestV2.setCountryCode("GB");

        when(modelMapper.map(Mockito.<Object>any(), Mockito.<Class<PackingRequestV2>>any())).thenReturn(packingRequestV2);
        var packing = new Packing();
        packing.setShipmentId(123L);
        packing.setConsolidationId(123L);
        var packingList = List.of(packing);
        var containerList = Containers.builder().containerNumber("CONT121212").build();
        containerList.setId(1L);
        var containers = List.of(containerList);

        UUID shipmentGuid = UUID.randomUUID();

        // Act
        List<PackingRequestV2> actualPackingsV2ToV1Result = syncEntityConversionService.packingsV2ToV1(packingList,
                containers, shipmentGuid, UUID.randomUUID());

        // Assert
        verify(modelMapper).map(isA(Object.class), isA(Class.class));
        assertEquals(1, actualPackingsV2ToV1Result.size());
    }

    @Test
    void testPackingsV2ToV122() {
        // Arrange
        PackingRequestV2 packingRequestV2 = new PackingRequestV2();
        packingRequestV2.setConsolidationGuid(UUID.randomUUID());
        packingRequestV2.setConsolidationId(1L);
        packingRequestV2.setContainerNumber("42");
        packingRequestV2.setCountryCode("GB");

        when(modelMapper.map(Mockito.<Object>any(), Mockito.<Class<PackingRequestV2>>any())).thenReturn(packingRequestV2);
        when(iShipmentDao.findById(anyLong())).thenReturn(Optional.of(new ShipmentDetails()));
        when(iConsolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(new ConsolidationDetails()));
        var packing = new Packing();
        packing.setContainerId(1L);
        packing.setShipmentId(123L);
        packing.setConsolidationId(123L);

        var packingList = List.of(packing);
        var containerList = Containers.builder().containerNumber("CONT121212").build();
        containerList.setId(1L);
        var containers = List.of(containerList);

        // Act
        List<PackingRequestV2> actualPackingsV2ToV1Result = syncEntityConversionService.packingsV2ToV1(packingList,
                containers, null, null);

        // Assert
        verify(modelMapper).map(isA(Object.class), isA(Class.class));
        assertEquals(1, actualPackingsV2ToV1Result.size());
    }

    @Test
    void packingsV1ToV2() {
        var inputPackingRequest = new PackingRequestV2();
        inputPackingRequest.setConsolidationGuid(UUID.randomUUID());
        inputPackingRequest.setShipmentGuid(UUID.randomUUID());

        when(modelMapper.map(any(), eq(Packing.class))).thenReturn(new Packing());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(iShipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));

        var response =  syncEntityConversionService.packingsV1ToV2(List.of(inputPackingRequest));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void packingsV1ToV2_2() {
        var inputPackingRequest = new PackingRequestV2();
        inputPackingRequest.setConsolidationGuid(UUID.randomUUID());
        inputPackingRequest.setShipmentGuid(UUID.randomUUID());

        when(modelMapper.map(any(), eq(Packing.class))).thenReturn(new Packing());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
        when(iShipmentDao.findByGuid(any())).thenReturn(Optional.empty());

        var response =  syncEntityConversionService.packingsV1ToV2(List.of(inputPackingRequest));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void packingsV1ToV2_3() {
        var inputPackingRequest = new PackingRequestV2();
        when(modelMapper.map(any(), eq(Packing.class))).thenReturn(new Packing());

        var response =  syncEntityConversionService.packingsV1ToV2(List.of(inputPackingRequest));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void packingsV1ToV2_4() {
        var response =  syncEntityConversionService.packingsV1ToV2(null);

        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void containersV2ToV1() {
        when(modelMapper.map(any(), eq(ContainerRequestV2.class))).thenReturn(new ContainerRequestV2());
        var response =  syncEntityConversionService.containersV2ToV1(List.of(new Containers()));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void containersV2ToV1_2() {
        var response =  syncEntityConversionService.containersV2ToV1(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }


    @Test
    void containersV1ToV2() {
        var response =  syncEntityConversionService.containersV1ToV2(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void containersV1ToV2_2() {
        var mockContainer = new ContainerRequestV2();
        mockContainer.setConsolidationGuid(UUID.randomUUID());
        mockContainer.setShipmentGuids(List.of(UUID.randomUUID()));
        when(modelMapper.map(any(), eq(Containers.class))).thenReturn(new Containers());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(iShipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new ShipmentDetails())));


        var response =  syncEntityConversionService.containersV1ToV2(List.of(mockContainer));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void containersV1ToV2_3() {
        var mockContainer = new ContainerRequestV2();
        mockContainer.setConsolidationGuid(UUID.randomUUID());
        mockContainer.setShipmentGuids(List.of(UUID.randomUUID()));
        when(modelMapper.map(any(), eq(Containers.class))).thenReturn(new Containers());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
        when(iShipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));


        var response =  syncEntityConversionService.containersV1ToV2(List.of(mockContainer));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void containersV1ToV2_4() {
        var mockContainer = new ContainerRequestV2();
        mockContainer.setConsolidationGuid(UUID.randomUUID());
        mockContainer.setShipmentGuids(List.of(UUID.randomUUID()));
        when(modelMapper.map(any(), eq(Containers.class))).thenReturn(new Containers());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(iShipmentDao.findAll(any(), any())).thenReturn(null);


        var response =  syncEntityConversionService.containersV1ToV2(List.of(mockContainer));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void containersV1ToV2_5() {
        var mockContainer = new ContainerRequestV2();
        mockContainer.setConsolidationGuid(UUID.randomUUID());
        mockContainer.setShipmentGuids(List.of());
        when(modelMapper.map(any(), eq(Containers.class))).thenReturn(new Containers());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(new ConsolidationDetails()));

        var response =  syncEntityConversionService.containersV1ToV2(List.of(mockContainer));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void containersV1ToV2_6() {
        var mockContainer = new ContainerRequestV2();
        when(modelMapper.map(any(), eq(Containers.class))).thenReturn(new Containers());

        var response =  syncEntityConversionService.containersV1ToV2(List.of(mockContainer));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void routingsV2ToV1() {
        var response = syncEntityConversionService.routingsV2ToV1(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void routingsV2ToV1_2() {
        when(modelMapper.map(any(), eq(RoutingsRequestV2.class))).thenReturn(new RoutingsRequestV2());
        var response = syncEntityConversionService.routingsV2ToV1(List.of(new Routings()));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void routingsV2ToV1_3() {
        var mockData = new RoutingsRequestV2();
        mockData.setMode(Constants.TRANSPORT_MODE_ROA);
        when(modelMapper.map(any(), eq(RoutingsRequestV2.class))).thenReturn(mockData);
        var response = syncEntityConversionService.routingsV2ToV1(List.of(new Routings()));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }


    @Test
    void routingsV1ToV2() {
        var response = syncEntityConversionService.routingsV1ToV2(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void routingsV1ToV2_2() {
        when(modelMapper.map(any(), eq(Routings.class))).thenReturn(new Routings());
        var response = syncEntityConversionService.routingsV1ToV2(List.of(new RoutingsRequestV2()));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void routingsV1ToV2_3() {
        var mockData = new RoutingsRequestV2();
        mockData.setMode(Constants.TRANSPORT_MODE_ROA);
        when(modelMapper.map(any(), eq(Routings.class))).thenReturn(new Routings());
        var response = syncEntityConversionService.routingsV1ToV2(List.of(mockData));

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void addressesV2ToV1() {
        var response = syncEntityConversionService.addressesV2ToV1(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void addressesV2ToV1_2() {
        var inputParty = new Parties();
        inputParty.setIsAddressFreeText(true);

        inputParty.setAddressData(new HashMap<>());

        var mockParty = new PartyRequestV2();

        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV2ToV1(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void addressesV2ToV1_3() {
        var inputParty = new Parties();
        inputParty.setIsAddressFreeText(true);

        var mockParty = new PartyRequestV2();

        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV2ToV1(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void addressesV2ToV1_4() {
        var inputParty = new Parties();

        var mockParty = new PartyRequestV2();

        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV2ToV1(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void addressesV2ToV1_5() {
        var inputParty = new Parties();
        inputParty.setIsAddressFreeText(true);
        var mockMap = new HashMap<String, Object>();
        mockMap.put(PartiesConstants.RAW_DATA, "DPW");
        inputParty.setAddressData(mockMap);

        var mockParty = new PartyRequestV2();

        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV2ToV1(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }


    @Test
    void addressesV1ToV2() {
        var response = syncEntityConversionService.addressesV1ToV2(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }


    @Test
    void addressesV1ToV2_2() {
        var inputParty = new PartyRequestV2();
        inputParty.setIsFreeTextAddress(true);

        var mockParty = new Parties();

        when(modelMapper.map(any(), eq(Parties.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV1ToV2(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void addressesV1ToV2_3() {
        var inputParty = new PartyRequestV2();
        inputParty.setIsFreeTextAddress(true);

        var mockParty = new Parties();
        mockParty.setAddressData(new HashMap<>());
        when(modelMapper.map(any(), eq(Parties.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV1ToV2(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void addressesV1ToV2_4() {
        var inputParty = new PartyRequestV2();

        var mockParty = new Parties();

        when(modelMapper.map(any(), eq(Parties.class))).thenReturn(mockParty);
        var response = syncEntityConversionService.addressesV1ToV2(List.of(inputParty));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void eventsV2ToV1() {
        var response = syncEntityConversionService.eventsV2ToV1(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void eventsV2ToV1_2() {
        var inputEvent1 = new Events();
        inputEvent1.setEntityType(Constants.SHIPMENT);

        var inputEvent2 = new Events();
        inputEvent2.setEntityType(Constants.CONSOLIDATION);

        var inputEvent3 = new Events();

        var mockEvent = new EventsRequestV2();

        when(modelMapper.map(any(), eq(EventsRequestV2.class))).thenReturn(mockEvent);
        when(iConsolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(iShipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));

        var response = syncEntityConversionService.eventsV2ToV1(List.of(inputEvent1, inputEvent2, inputEvent3));
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void mawbStocksV2ToV1() {
        var inputStock = new MawbStocks();
        inputStock.setConsolidationId(123L);

        var mockStockV2 = new MawbStocksV2();
        var mawbStockLink1 = new MawbStocksLinkV2();
        mawbStockLink1.setEntityType(Constants.SHIPMENT);

        var mawbStockLink2 = new MawbStocksLinkV2();
        mawbStockLink2.setEntityType(Constants.CONSOLIDATION);

        var mawbStockLink3 = new MawbStocksLinkV2();

        mockStockV2.setMawbStocksLinkRows(List.of(mawbStockLink1, mawbStockLink2, mawbStockLink3));
        var mockConsole = new ConsolidationDetails();
        mockConsole.setGuid(UUID.randomUUID());
        var mockShipment = new ShipmentDetails();
        mockShipment.setGuid(UUID.randomUUID());
        when(iConsolidationDetailsDao.findById(any())).thenReturn(Optional.of(mockConsole));
        when(iShipmentDao.findById(any())).thenReturn(Optional.of(mockShipment));
        when(modelMapper.map(any(), eq(MawbStocksV2.class))).thenReturn(mockStockV2);

        var response = syncEntityConversionService.mawbStocksV2ToV1(inputStock);

        assertNotNull(response);
    }

    @Test
    void mawbStocksV2ToV1_2() {
        var inputStock = new MawbStocks();

        var mockStockV2 = new MawbStocksV2();

        var mockConsole = new ConsolidationDetails();
        mockConsole.setGuid(UUID.randomUUID());
        var mockShipment = new ShipmentDetails();
        mockShipment.setGuid(UUID.randomUUID());
        when(modelMapper.map(any(), eq(MawbStocksV2.class))).thenReturn(mockStockV2);

        var response = syncEntityConversionService.mawbStocksV2ToV1(inputStock);

        assertNotNull(response);
    }

    @Test
    void mawbStocksV1ToV2() {
        var inputStock = new MawbStocksV2();
        inputStock.setConsolidationGuid(UUID.randomUUID());

        var mockStockV2 = new MawbStocks();
        var mawbStockLink1 = new MawbStocksLinkV2();
        mawbStockLink1.setEntityType(Constants.SHIPMENT);

        var mawbStockLink2 = new MawbStocksLinkV2();
        mawbStockLink2.setEntityType(Constants.CONSOLIDATION);

        var mawbStockLink3 = new MawbStocksLinkV2();

        inputStock.setMawbStocksLinkRows(List.of(mawbStockLink1, mawbStockLink2, mawbStockLink3));
        var mockConsole = new ConsolidationDetails();
        mockConsole.setGuid(UUID.randomUUID());
        var mockShipment = new ShipmentDetails();
        mockShipment.setGuid(UUID.randomUUID());
        when(iConsolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(mockConsole));
        when(iShipmentDao.findByGuid(any())).thenReturn(Optional.of(mockShipment));
        when(modelMapper.map(any(), eq(MawbStocks.class))).thenReturn(mockStockV2);

        var response = syncEntityConversionService.mawbStocksV1ToV2(inputStock);

        assertNotNull(response);
    }
    
}
