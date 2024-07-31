package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.entity.*;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.commons.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.commons.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.commons.dto.request.UsersDto;
import com.dpw.runner.shipment.services.commons.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.commons.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ConsolidationDaoTest extends CommonMocks {

    @Mock
    private IConsolidationRepository consolidationRepository;

    @Mock
    IShipmentRepository shipmentRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IMawbStocksDao mawbStocksDao;

    @Mock
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Mock
    private IV1Service v1Service;
    @InjectMocks
    private ConsolidationDao consolidationsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ConsolidationDetails testConsol;

    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetailsRequest testConsolRequest;

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
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testConsol = jsonTestUtility.getTestConsolidation();
        testConsolRequest = objectMapperTest.convertValue(testConsol , ConsolidationDetailsRequest.class);
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
    }

    @Test
    void testSave_Success_Sea() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails).when(consolidationRepository).save(any());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.save(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testSave_Success_Air() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        CarrierResponse carrierResponse = CarrierResponse.builder().airlineCode("390").itemValue("Aegean Airlines").hasAirPort(true).build();
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(List.of(carrierResponse)).build();
        MawbStocks mawbStocks = jsonTestUtility.getMawbStock();
        MawbStocksLink mawbStocksLink = jsonTestUtility.getNewMawbStockLink();
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(v1DataResponse).when(v1Service).fetchCarrierMasterData(any(), anyBoolean());
        doReturn(List.of(carrierResponse)).when(jsonHelper).convertValueToList(List.of(carrierResponse), CarrierResponse.class);
        doReturn(consolidationDetails).when(consolidationRepository).save(any());
        doReturn(new PageImpl<>(List.of())).when(mawbStocksLinkDao).findAll(any(), any());
        doReturn(mawbStocks).when(mawbStocksDao).save(any());
        doReturn(mawbStocksLink).when(mawbStocksLinkDao).save(any());
        doReturn(List.of(mawbStocksLink)).when(mawbStocksLinkDao).findByMawbNumber(anyString());
        doReturn(Optional.of(mawbStocks)).when(mawbStocksDao).findById(anyLong());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.save(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testSave_Failure_Air_Validations1() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainsHazardous(true);
        consolidationDetails.setShipmentsList(List.of(shipmentDetails));
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainsHazardous(false);
        consolidationDetails.setShipmentsList(List.of(shipmentDetails));
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.airDG, true);
        UserContext.getUser().setPermissions(permissions);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations_nullShipments() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        consolidationDetails.setShipmentsList(null);
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.airDG, true);
        UserContext.getUser().setPermissions(permissions);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations1_NullPacks() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(null);
        consolidationDetails.setShipmentsList(List.of(jsonTestUtility.getTestShipment()));
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations1_NullPacks_HazardousShipment() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(null);
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainsHazardous(true);
        consolidationDetails.setShipmentsList(List.of(shipmentDetails));
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations_HazFalse() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations_HazFalse_HazardousPack() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        consolidationDetails.getPackingList().get(0).setHazardous(true);
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations_HazTrue() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setHazardous(true);
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations2_EXP() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setRestrictedLocationsEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations2_IMP() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setShipmentType(Constants.IMP);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setRestrictedLocationsEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Sea_Validations3_Bol() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getTestConsolidation();
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(List.of(consolidationDetails1)).when(spyService).findByBol(consolidationDetails.getBol());
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_ETA_ETD() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.getCarrierDetails().setEta(LocalDateTime.parse("2024-05-09T20:50:36"));
        consolidationDetails.getCarrierDetails().setEtd(LocalDateTime.parse("2024-05-11T20:50:36"));
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_SEA_ETA_ETD() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.getCarrierDetails().setEta(LocalDateTime.parse("2024-05-09T20:50:36"));
        consolidationDetails.getCarrierDetails().setEtd(LocalDateTime.parse("2024-05-11T20:50:36"));
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testUpdate_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails).when(consolidationRepository).save(any());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.update(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testFindAll_Success() {
        Page<ConsolidationDetails> consolidationPage = mock(Page.class);
        Specification<ConsolidationDetails> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(consolidationRepository.findAll(spec, pageable)).thenReturn(consolidationPage);
        Page<ConsolidationDetails> consolidationDetails = consolidationsDao.findAll(spec, pageable);
        assertEquals(consolidationPage, consolidationDetails);
    }

    @Test
    void testFindById_Success() {
        Optional<ConsolidationDetails> optionalConsolidationDetails = Optional.of(testConsol);
        when(consolidationRepository.findById(anyLong())).thenReturn(optionalConsolidationDetails);
        Optional<ConsolidationDetails> consolidationDetails = consolidationsDao.findById(1L);
        assertTrue(consolidationDetails.isPresent());
        assertEquals(testConsol, consolidationDetails.get());
    }

    @Test
    void testDelete_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        assertDoesNotThrow(() -> consolidationsDao.delete(consolidationDetails));
        verify(consolidationRepository, Mockito.times(1)).delete(consolidationDetails);
    }

    @Test
    void testDelete_Failure() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setIsLocked(true);
        assertThrows(ValidationException.class, () -> consolidationsDao.delete(consolidationDetails));
    }

    @Test
    void testSaveAll_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(consolidationDetails).when(spyService).save(consolidationDetails, false);
        List<ConsolidationDetails> responseEntity = spyService.saveAll(List.of(consolidationDetails));
        assertEquals(List.of(consolidationDetails), responseEntity);
    }

    @Test
    void testFindByGuid_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationRepository.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        Optional<ConsolidationDetails> responseEntity = consolidationsDao.findByGuid(consolidationDetails.getGuid());
        assertTrue(responseEntity.isPresent());
        assertEquals(consolidationDetails, responseEntity.get());
    }

    @Test
    void testFindByBol_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationRepository.findByBol(anyString())).thenReturn(List.of(consolidationDetails));
        List<ConsolidationDetails> responseEntity = consolidationsDao.findByBol(consolidationDetails.getBol());
        assertEquals(List.of(consolidationDetails), responseEntity);
    }

    @Test
    void testFindByReferenceNumber_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationRepository.findByReferenceNumber(anyString())).thenReturn(List.of(consolidationDetails));
        List<ConsolidationDetails> responseEntity = consolidationsDao.findByReferenceNumber(consolidationDetails.getReferenceNumber());
        assertEquals(List.of(consolidationDetails), responseEntity);
    }

    @Test
    void testFindMaxId_Success() {
        when(consolidationRepository.findMaxId()).thenReturn(123L);
        Long val = consolidationsDao.findMaxId();
        assertEquals(123L, val);
    }

    @Test
    void testUpdateConsoleBookingFields_Success() {
        ConsoleBookingRequest request = ConsoleBookingRequest.builder().guid(UUID.randomUUID()).bookingId("123").bookingNumber("Booking123").bookingStatus("xyz").build();
        when(consolidationRepository.updateConsoleBookingFields(any(),anyString(),anyString(),anyString())).thenReturn(1);
        int response = consolidationsDao.updateConsoleBookingFields(request);
        assertEquals(1, response);
    }

    @Test
    void testSaveCreatedDateAndUser_Success() {
        consolidationsDao.saveCreatedDateAndUser(1L, "user", LocalDateTime.parse("2024-05-09T20:50:36"));
        verify(consolidationRepository, Mockito.times(1)).saveCreatedDateAndUser(1L, "user", LocalDateTime.parse("2024-05-09T20:50:36"));
    }

    @Test
    void testGetConsolidationNumberFromId_Success() {
        String ConsolidationNumber = "CONS000233659";
        when(consolidationRepository.getConsolidationNumberFromId(anyLong())).thenReturn(ConsolidationNumber);
        String response = consolidationsDao.getConsolidationNumberFromId(1L);
        assertEquals(ConsolidationNumber, response);
    }

    @Test
    void testUpdateMawbExp() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType("EXP");

        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails).when(consolidationRepository).save(any());
        doNothing().when(mawbStocksLinkDao).deLinkExistingMawbStockLink(any());
        mockShipmentSettings();

        ConsolidationDetails responseEntity = spyService.update(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testUpdateMawbInvalidMawb() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setMawb("mawb");
        consolidationDetails.setShipmentType("IMP");

        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> {
            spyService.update(consolidationDetails, false);
        });

    }

    @Test
    void testUpdateMawbValidMawbCarrierImp() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setMawb("MAST77777770");
        consolidationDetails.setShipmentType("IMP");

        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails).when(consolidationRepository).save(any());

        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.update(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testUpdateMawbValidMawbCarrierResponseNull() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setMawb("MAST77777770");
        consolidationDetails.setShipmentType("IMP");
        consolidationDetails.getCarrierDetails().setShippingLine(null);

        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());

        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(CarrierResponse.class))).thenReturn(null);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> {
            spyService.update(consolidationDetails, false);
        });
    }

    @Test
    void testUpdateMawbValidMawbCarrierResponseNotNull() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setMawb("MAST77777770");
        consolidationDetails.setShipmentType("EXP");
        consolidationDetails.getCarrierDetails().setShippingLine(null);

        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());

        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(CarrierResponse.class))).thenReturn(Arrays.asList(CarrierResponse.builder().build()));
        doReturn(new PageImpl<>(List.of(MawbStocksLink.builder().status("Consumed").build()))).when(mawbStocksLinkDao).findAll(any(), any());
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> {
            spyService.update(consolidationDetails, false);
        });
    }

    @Test
    void checkSameMblExists() {
        assertFalse(consolidationsDao.checkSameMblExists(null, null));
    }

    @Test
    void checkSameMblExists_RequestIdNotNull() {
        testConsol.setId(3L);
        assertFalse(consolidationsDao.checkSameMblExists(List.of(testConsol), testConsol));
    }

    @Test
    void checkSameMblExists_RequestIdNotNull_DifferentId() {
        testConsol.setId(3L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(6L);
        assertTrue(consolidationsDao.checkSameMblExists(List.of(testConsol), consolidationDetails));
    }

    @Test
    void checkSameMblExists_MoreThanOneExists() {
        testConsol.setId(3L);
        assertTrue(consolidationsDao.checkSameMblExists(List.of(testConsol, testConsol), testConsol));
    }
    @Test
    void testFindConsolidationsByGuids() {
        when(consolidationRepository.findConsolidationsByGuids(Set.of(testConsol.getGuid()))).thenReturn(List.of(testConsol));
        var response = consolidationsDao.findConsolidationsByGuids(Set.of(testConsol.getGuid()));
        assertEquals(List.of(testConsol), response);
    }
}