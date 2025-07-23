package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection.NullConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;

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
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IV1Service v1Service;
    @InjectMocks
    private ConsolidationDao consolidationsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ConsolidationDetails testConsol;


    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testConsol = jsonTestUtility.getTestConsolidation();
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
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
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
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
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
        permissions.put(PermissionConstants.AIR_DG, true);
        UserContext.getUser().setPermissions(permissions);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.save(consolidationDetails, false));
    }

    @Test
    void testSave_Failure_Air_Validations1_NullPacks() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(null);
        consolidationDetails.setShipmentsList(Set.of(jsonTestUtility.getTestShipment()));
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
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
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
        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setInterBranchConsole(false);
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails2).when(consolidationRepository).save(any());
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
        when(consolidationRepository.findByBol(anyString(), any())).thenReturn(List.of(consolidationDetails));
        List<ConsolidationDetails> responseEntity = consolidationsDao.findByBol(consolidationDetails.getBol());
        assertEquals(List.of(consolidationDetails), responseEntity);
    }

    @Test
    void testFindByReferenceNumber_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationRepository.findByReferenceNumber(anyString(), any())).thenReturn(List.of(consolidationDetails));
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
        MawbStocks mawbStocks = jsonTestUtility.getMawbStock();

        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails).when(consolidationRepository).save(any());

        doReturn(new PageImpl<>(List.of())).when(mawbStocksLinkDao).findAll(any(), any());
        doReturn(mawbStocks).when(mawbStocksDao).save(any());
        mockShipmentSettings();

        ConsolidationDetails responseEntity = spyService.update(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testUpdateMawbInvalidMawb() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setMawb("mawb");
        consolidationDetails.setBol("bol");
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
    @Test
    void testFindConsolidationsByIds() {
        when(consolidationRepository.findConsolidationsByIds(Set.of(testConsol.getId()))).thenReturn(List.of(testConsol));
        var response = consolidationsDao.findConsolidationsByIds(Set.of(testConsol.getId()));
        assertEquals(List.of(testConsol), response);
    }
    @Test
    void testFindConsolidationsById() {
        when(consolidationRepository.getConsolidationFromId(testConsol.getId())).thenReturn(testConsol);
        var response = consolidationsDao.findConsolidationsById(testConsol.getId());
        assertEquals(testConsol, response);
    }

    @Test
    void testFindMblNumberInDifferentTenant() {
        try (MockedStatic<TenantContext> mockedTenantContext = Mockito.mockStatic(TenantContext.class)) {
            ConsolidationDetailsProjection nullProjection = new NullConsolidationDetailsProjection();

            List<ConsolidationDetailsProjection> projections = new ArrayList<>();
            projections.add(nullProjection);

            String sampleMblNumber = "sampleMblNumber";
            Integer tenantId = 1;

            mockedTenantContext.when(TenantContext::getCurrentTenant).thenReturn(tenantId);

            when(consolidationRepository.findMblNumberInDifferentTenant(sampleMblNumber, tenantId))
                    .thenReturn(projections);

            var response = consolidationsDao.findMblNumberInDifferentTenant(sampleMblNumber);

            assertEquals(projections, response);
        }
    }

    @Test
    void testUpdate_Success2() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails2.setInterBranchConsole(false);
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails2).when(consolidationRepository).save(any());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.update(consolidationDetails, false);
        assertNotNull(responseEntity);
    }

    @Test
    void testUpdate_Success3() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails.setOpenForAttachment(true);
        consolidationDetails2.setOpenForAttachment(false);
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails2).when(consolidationRepository).save(any());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.update(consolidationDetails, false);
        assertNotNull(responseEntity);
    }

    @Test
    void findBySourceGuid() {
        when(consolidationRepository.findBySourceGuid(any())).thenReturn(List.of(new ConsolidationDetails()));
        var response = consolidationsDao.findBySourceGuid(UUID.randomUUID());
        assertFalse(response.isEmpty());
    }

    @Test
    void testGetIdWithPendingActions() {
        List<Long> eligibleId = List.of(1L, 2L, 3L);
        Page<Long> consolIdPage = new PageImpl<>(eligibleId);
        when(consolidationRepository.getIdWithPendingActions(any(), any())).thenReturn(consolIdPage);
        var response = consolidationsDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED, PageRequest.of(1, 25));

        assertEquals(consolIdPage, response);
    }

    @Test
    void findConsolidationByIdWithQuery() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationRepository.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.of(consolidationDetails));
        Optional<ConsolidationDetails> responseEntity = consolidationsDao.findConsolidationByIdWithQuery(consolidationDetails.getId());
        assertEquals(Optional.of(consolidationDetails), responseEntity);
    }

    @Test
    void applyConsolidationValidationsTest_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .shipmentType(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidations(consolidationDetails, false, false);
        assertTrue(errors.contains("The consolidation contains DG package. Marking the consolidation as non DG is not allowed"));
    }

    @Test
    void applyConsolidationValidationsTest_CountryAirCargoSecurity2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .shipmentType(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidations(consolidationDetails, false, false);
        assertTrue(errors.contains("First load or Last Discharge can not be null."));
    }

    @Test
    void applyConsolidationValidationsExpTest_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().containsHazardous(true).build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .shipmentsList(Set.of(shipmentDetails))
                .shipmentType(Constants.DIRECTION_EXP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidations(consolidationDetails, false, false);
        assertTrue(errors.contains("You don't have Air Security permission to create or update AIR EXP Consolidation."));
    }

    @Test
    void applyConsolidationValidationsExpTest_CountryAirCargoSecurity2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().containsHazardous(false).build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(true)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .shipmentsList(Set.of(shipmentDetails))
                .shipmentType(Constants.DIRECTION_EXP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidations(consolidationDetails, false, false);
        assertTrue(errors.contains("Consolidation cannot be marked as DG. Please attach at least one DG Shipment."));
    }

    @Test
    void applyConsolidationValidationsExpTest_CountryAirCargoSecurity3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().containsHazardous(false).build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(true)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .shipmentsList(Set.of(shipmentDetails))
                .shipmentType(Constants.DIRECTION_EXP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidations(consolidationDetails, true, false);
        assertTrue(errors.contains("First load or Last Discharge can not be null."));
    }

    @Test
    void applyShipmentValidationsTest_NonHazPack_CountryAirCargoSecurity_V1Sync() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .shipmentType(Constants.DIRECTION_EXP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidations(consolidationDetails, false, true);
        assertFalse(errors.contains("You don't have Air Security permission to create or update AIR EXP Consolidation."));
    }

    @Test
    void testSaveV3_Success_Sea() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails).when(consolidationRepository).save(any());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.saveV3(consolidationDetails);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testSaveV3_Success_Air() {
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
        ConsolidationDetails responseEntity = spyService.saveV3(consolidationDetails);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void testSaveV3_Failure_Air_Validations1() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainsHazardous(true);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainsHazardous(false);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        UserContext.getUser().setPermissions(permissions);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations_nullShipments() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        consolidationDetails.setShipmentsList(null);
        var spyService = Mockito.spy(consolidationsDao);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        UserContext.getUser().setPermissions(permissions);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations1_NullPacks() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(null);
        consolidationDetails.setShipmentsList(Set.of(jsonTestUtility.getTestShipment()));
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations1_NullPacks_HazardousShipment() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setPackingList(null);
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainsHazardous(true);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations_HazFalse() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations_HazFalse_HazardousPack() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        consolidationDetails.getPackingList().get(0).setHazardous(true);
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations_HazTrue() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setHazardous(true);
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setConsolidationAddresses(jsonTestUtility.getConsoldiationAddressList());
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations2_EXP() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setRestrictedLocationsEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_Validations2_IMP() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.setShipmentType(Constants.IMP);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setRestrictedLocationsEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Sea_Validations3_Bol() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getTestConsolidation();
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(List.of(consolidationDetails1)).when(spyService).findByBol(consolidationDetails.getBol());
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_Air_ETA_ETD() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.getCarrierDetails().setEta(LocalDateTime.parse("2024-05-09T20:50:36"));
        consolidationDetails.getCarrierDetails().setEtd(LocalDateTime.parse("2024-05-11T20:50:36"));
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure_SEA_ETA_ETD() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setId(null);
        consolidationDetails.setGuid(null);
        consolidationDetails.getCarrierDetails().setEta(LocalDateTime.parse("2024-05-09T20:50:36"));
        consolidationDetails.getCarrierDetails().setEtd(LocalDateTime.parse("2024-05-11T20:50:36"));
        var spyService = Mockito.spy(consolidationsDao);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testSaveV3_Failure() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveV3(consolidationDetails));
    }

    @Test
    void testUpdateV3_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setInterBranchConsole(false);
        var spyService = Mockito.spy(consolidationsDao);
        doReturn(Optional.of(consolidationDetails)).when(spyService).findById(anyLong());
        doReturn(consolidationDetails2).when(consolidationRepository).save(any());
        mockShipmentSettings();
        ConsolidationDetails responseEntity = spyService.updateV3(consolidationDetails, false);
        assertEquals(consolidationDetails, responseEntity);
    }

    @Test
    void applyConsolidationValidationsTest_CountryAirCargoSecurityV3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .shipmentType(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertTrue(errors.contains("The consolidation contains DG package. Marking the consolidation as non DG is not allowed"));
    }

    @Test
    void applyAgentOrganisationIdValidationTest_SendingAgentNullOnlyV3() {

        Parties receivingAgent = mock(Parties.class);
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setSendingAgent(null);
        consolidationDetails.setReceivingAgent(receivingAgent);
        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertFalse(errors.contains("Origin Agent and Destination Agent cannot be same Organisation."));
    }

    @Test
    void applyAgentOrganisationIdValidationTest_ReceivingAgentNullOnlyV3() {

        Parties sendingAgent = mock(Parties.class);
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setSendingAgent(sendingAgent);
        consolidationDetails.setReceivingAgent(null);
        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertFalse(errors.contains("Origin Agent and Destination Agent cannot be same Organisation."));
    }

    @Test
    void applyAgentOrganisationIdValidationTest_BothAgentsNullV3() {

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setSendingAgent(null);
        consolidationDetails.setReceivingAgent(null);
        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertFalse(errors.contains("Origin Agent and Destination Agent cannot be same Organisation."));
    }

    @Test
    void applyAgentOrganisationIdValidationTest_SameOrganisationsFailureV3() {

        Parties sendingAgent = mock(Parties.class);
        Parties receivingAgent = mock(Parties.class);
        when(sendingAgent.getOrgId()).thenReturn("123");
        when(receivingAgent.getOrgId()).thenReturn("123");
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setSendingAgent(sendingAgent);
        consolidationDetails.setReceivingAgent(receivingAgent);
        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertTrue(errors.contains("Origin Agent and Destination Agent cannot be same Organisation."));
    }

    @Test
    void applyAgentOrganisationIdValidationTest_DifferentOrganisationsSuccessV3() {

        Parties sendingAgent = mock(Parties.class);
        Parties receivingAgent = mock(Parties.class);
        when(sendingAgent.getOrgId()).thenReturn("324");
        when(receivingAgent.getOrgId()).thenReturn("123");
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setSendingAgent(sendingAgent);
        consolidationDetails.setReceivingAgent(receivingAgent);
        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertFalse(errors.contains("Origin Agent and Destination Agent cannot be same Organisation."));
    }

    @Test
    void applyConsolidationValidationsTest_CountryAirCargoSecurity2V3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .shipmentType(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertTrue(errors.contains("First load or Last Discharge can not be null."));
    }

    @ParameterizedTest
    @MethodSource("provideConsolidationValidationScenarios")
    void applyConsolidationValidationsExpTest_CountryAirCargoSecurityV3Parameterized(
        boolean shipmentHazardous,
        boolean consolidationHazardous,
        String expectedError
    ) {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
            ShipmentSettingsDetails.builder()
                .countryAirCargoSecurity(true)
                .restrictedLocationsEnabled(true)
                .build()
        );

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
            .containsHazardous(shipmentHazardous)
            .build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
            .hazardous(consolidationHazardous)
            .transportMode(Constants.TRANSPORT_MODE_AIR)
            .shipmentsList(Set.of(shipmentDetails))
            .shipmentType(Constants.DIRECTION_EXP)
            .build();

        mockShipmentSettings(); // Ensure this is available as a mock/stub in the test context
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);

        assertTrue(errors.contains(expectedError));
    }

    static Stream<Arguments> provideConsolidationValidationScenarios() {
        return Stream.of(
            Arguments.of(
                true, // shipmentHazardous
                false, // consolidationHazardous
                "You don't have Air Security permission to create or update AIR EXP Consolidation."
            ),
            Arguments.of(
                false,
                true,
                "Consolidation cannot be marked as DG. Please attach at least one DG Shipment."
            ),
            Arguments.of(
                false,
                true,
                "First load or Last Discharge can not be null."
            )
        );
    }

    @Test
    void applyShipmentValidationsTest_NonHazPack_CountryAirCargoSecurity_V1SyncV3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .hazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .shipmentType(Constants.DIRECTION_EXP)
                .build();

        mockShipmentSettings();
        Set<String> errors = consolidationsDao.applyConsolidationValidationsV3(consolidationDetails, false);
        assertTrue(errors.contains("You don't have Air Security permission to create or update AIR EXP Consolidation."));
    }

}