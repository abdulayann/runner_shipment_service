package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.LicenseContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection.NullShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ShipmentDaoTest extends CommonMocks {

    @InjectMocks
    private ShipmentDao shipmentDao;

    @Mock
    private EntityManager entityManager;

    @Mock
    private IShipmentRepository shipmentRepository;
    @Mock
    private ValidatorUtility validatorUtility;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IMawbStocksLinkDao mawbStocksLinkDao;
    @Mock
    private IV1Service v1Service;
    @Mock
    private IMawbStocksDao mawbStocksDao;
    @Mock
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Test
    void testFindByHblNumberAndExcludeShipmentId() {
        try (MockedStatic<TenantContext> mockedTenantContext = Mockito.mockStatic(TenantContext.class)) {
            ShipmentDetailsProjection nullProjection = new NullShipmentDetailsProjection();

            List<ShipmentDetailsProjection> projections = new ArrayList<>();
            projections.add(nullProjection);

            String sampleHblNumber = "sampleHblNumber";
            String sampleShipmentId = "sampleShipmentId";
            Integer tenantId = 1;

            mockedTenantContext.when(TenantContext::getCurrentTenant).thenReturn(tenantId);

            when(shipmentRepository.findByHblNumberAndExcludeShipmentId(sampleHblNumber, sampleShipmentId))
                    .thenReturn(projections);

            var response = shipmentDao.findByHblNumberAndExcludeShipmentId(sampleHblNumber, sampleShipmentId);

            assertEquals(projections, response);
        }
    }

    @Test
    void saveTestCatch() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(Containers.builder().build())));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertThrows(RunnerException.class, () -> {
            shipmentDao.save(shipmentDetails, false);
        });
    }

    @Test
    void saveTestOldEntityNotPresent() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(Containers.builder().build())));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentDao.save(shipmentDetails, false);
        });
    }

    @Test
    void saveTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(Containers.builder().build())));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void saveStatusTest() {
        Long shipmentId = 1L;
        Integer status = 1;

        doNothing().when(shipmentRepository).saveStatus(shipmentId, status);

        shipmentDao.saveStatus(shipmentId, status);

        verify(shipmentRepository, times(1)).saveStatus(shipmentId, status);
    }

    @Test
    void saveTest2() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(Containers.builder().build())));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void saveTest3() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(Containers.builder().build())));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void saveTest4() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setStatus(ShipmentStatus.GenerateHAWB.getValue());
        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void applyShipmentValidationsTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsExpTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(false)
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_HazShipment() {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true)
                    .build());

            Packing packing = new Packing();
            packing.setHazardous(false);

            Routings routings = new Routings();
            routings.setLeg(1L);

            Containers containers = Containers.builder().containerNumber("CON123").build();
            Parties parties = Parties.builder().type("type").build();

            ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
            consolidationDetails.setId(1L);

            ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(
                    Arrays.asList(ConsolidationDetails.builder().build(),
                        ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(
                    new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(true)
                .build();

            List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
            consolidationDetailsList.add(consolidationDetails);

            when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

            UsersDto usersDto = new UsersDto();
            Map<String, Boolean> permissions = new HashMap<>();
            permissions.put(PermissionConstants.AIR_DG, true);
            usersDto.setPermissions(permissions);
            UserContext.setUser(usersDto);
            mockShipmentSettings();
            Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
            assertFalse(
                errors.contains("Container Number cannot be same for two different containers"));
        }
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_NonDgUser() {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isDgAirLicense).thenReturn(true);
            ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true)
                    .build());

            Packing packing = new Packing();
            packing.setHazardous(false);

            Routings routings = new Routings();
            routings.setLeg(1L);

            Containers containers = Containers.builder().containerNumber("CON123").build();
            Parties parties = Parties.builder().type("type").build();

            ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
            consolidationDetails.setId(1L);

            ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(
                    Arrays.asList(ConsolidationDetails.builder().build(),
                        ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(
                    new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(true)
                .build();

            List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
            consolidationDetailsList.add(consolidationDetails);

            when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

            UsersDto usersDto = new UsersDto();
            Map<String, Boolean> permissions = new HashMap<>();
            usersDto.setPermissions(permissions);
            UserContext.setUser(usersDto);
            mockShipmentSettings();
            Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
            assertFalse(
                errors.contains("Container Number cannot be same for two different containers"));
        }
    }

    @Test
    void updateShipmentLockedTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().isLocked(true).build();
        assertThrows(ValidationException.class, () -> {
            shipmentDao.update(shipmentDetails, true);
        });
    }

    @Test
    void updateEmptyShipmentTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentDao.update(shipmentDetails, true);
        });
    }

    @Test
    void updateTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(CarrierDetails.builder().origin("origin").originPort("origin").destination("origin").destinationPort("destination").build())
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(shipmentDetails)).thenReturn(shipmentDetails);
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, true);
        assertNotNull(response);
    }

    @Test
    void updateHblNumberTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .status(3)
                .carrierDetails(CarrierDetails.builder().origin("origin").originPort("origin").destination("origin").destinationPort("destination").etd(LocalDateTime.now()).etd(LocalDateTime.now()).build())
                .jobType("DRT")
                .direction("EXP")
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(shipmentDetails)).thenReturn(shipmentDetails);

        doNothing().when(mawbStocksLinkDao).deLinkExistingMawbStockLink(any());
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @Test
    void updateHblNumberIMPTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("MBL123")
                .status(3)
                .carrierDetails(CarrierDetails.builder().origin("origin").originPort("origin").destination("origin").destinationPort("destination").etd(LocalDateTime.now()).etd(LocalDateTime.now()).build())
                .jobType("DRT")
                .direction("IMP")
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> {
            shipmentDao.update(shipmentDetails, false);
        });
    }

    @Test
    void updateHblNumberMasterBillEmptyEXPTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .shippingLine("Shipping")
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);


        doNothing().when(mawbStocksLinkDao).deLinkExistingMawbStockLink(any());
        when(v1Service.fetchCarrierMasterData(any(), eq(true))).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().build()));
        mockShipmentSettings();
        assertThrows(ValidationException.class, () ->{
            shipmentDao.update(shipmentDetails, false);
        });
    }

    @Test
    void updateHblNumberMasterBillEmptyEXPIataTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .shippingLine("Shipping")
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);


        doNothing().when(mawbStocksLinkDao).deLinkExistingMawbStockLink(any());
        when(v1Service.fetchCarrierMasterData(any(), eq(true))).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        mockShipmentSettings();
       ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
       assertNotNull(response);
    }

    @Test
    void updateMawbCheckTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().status(Constants.UNUSED).build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @Test
    void updateMawbCheckEmptyLinkPageTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        HashMap<String, Object> hm = new HashMap<>();
        hm.put("FullName", "DP World");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgData(hm).build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());

        MawbStocks mawbStocks = MawbStocks.builder().build();
        mawbStocks.setId(1L);

        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.save(any())).thenReturn(MawbStocksLink.builder().build());
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @Test
    void updateHBLTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        HashMap<String, Object> hm = new HashMap<>();
        hm.put("FullName", "DP World");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgData(hm).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(2)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails2));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());

        MawbStocks mawbStocks = MawbStocks.builder().build();
        mawbStocks.setId(1L);

        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.save(any())).thenReturn(MawbStocksLink.builder().build());
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @ParameterizedTest
    @CsvSource({
            "DRT, null",
            "DRT, STD",
            "STD, DRT",
            "DRT, DRT",
            "DRT, null",
            "null, DRT"
    })
    void updateMawbChecks(String jobType1, String jobType2) {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build()
        );

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        HashMap<String, Object> hm = new HashMap<>();
        hm.put("FullName", "DP World");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgData(hm).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(2)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType(jobType1)
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType(jobType2)
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails2));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());

        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        MawbStocks mawbStocks = MawbStocks.builder().build();
        mawbStocks.setId(1L);
        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.save(any())).thenReturn(MawbStocksLink.builder().build());

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);
        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @ParameterizedTest
    @CsvSource({
            "null, STD",
            "STD, STD",
            "STD, null"
    })
    void updateMawbChecks1(String jobType1, String jobType2) {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        HashMap<String, Object> hm = new HashMap<>();
        hm.put("FullName", "DP World");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgData(hm).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(2)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType(jobType1)
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType(jobType2)
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails2));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @Test
    void updateEtaEtdTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        HashMap<String, Object> hm = new HashMap<>();
        hm.put("FullName", "DP World");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgData(hm).build());

        LocalDateTime eta = LocalDateTime.now();
        LocalDateTime etd = eta.plusHours(25);


        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(2)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .eta(eta)
                        .etd(etd)
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .eta(eta)
                        .etd(etd)
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails2));

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        MawbStocks mawbStocks = MawbStocks.builder().build();
        mawbStocks.setId(1L);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> {
            shipmentDao.update(shipmentDetails, false);
        });
    }

    @Test
    void updateMawbStockTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        HashMap<String, Object> hm = new HashMap<>();
        hm.put("FullName", "DP World");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgData(hm).build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("origin")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any())).thenReturn(shipmentDetails);

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());

        MawbStocks mawbStocks = MawbStocks.builder().build();
        mawbStocks.setId(1L);

        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.save(any())).thenReturn(MawbStocksLink.builder().build());

        mawbStocksLinkList.add(MawbStocksLink.builder().status("CONSUMED").build());
        when(mawbStocksLinkDao.findByMawbNumber(any())).thenReturn(mawbStocksLinkList);
        when(mawbStocksDao.findById(any())).thenReturn(Optional.of(MawbStocks.builder().build()));
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @Test
    void saveJobStatus() {
        shipmentDao.saveJobStatus(1L, "Status");
        verify(shipmentRepository, times(1)).saveJobStatus(any(), any());
    }

    @Test
    void saveCreatedDateAndUser() {
        shipmentDao.saveCreatedDateAndUser(1L, "user", LocalDateTime.now());
        verify(shipmentRepository, times(1)).saveCreatedDateAndUser(any(), any(), any());
    }

    @Test
    void getShipmentNumberFromId() {
        shipmentDao.getShipmentNumberFromId(Arrays.asList(1L));
        verify(shipmentRepository, times(1)).getShipmentNumberFromId(any());
    }

    @Test
    void saveEntityTransfer() {
        shipmentDao.saveEntityTransfer(1L, true);
        verify(shipmentRepository, times(1)).saveEntityTransfer(any(), eq(true));
    }

    @Test
    void findShipmentsByGuids() {
        Set<UUID> request = Set.of(UUID.randomUUID());
        shipmentDao.findShipmentsByGuids(request);
        verify(shipmentRepository, times(1)).findShipmentsByGuids(request);
    }

    @Test
    void findShipmentsBySourceGuids() {
        Set<UUID> request = Set.of(UUID.randomUUID());
        shipmentDao.findShipmentsBySourceGuids(request);
        verify(shipmentRepository, times(1)).findShipmentsBySourceGuids(request);
    }

    @Test
    void findShipmentsByIds() {
        Set<Long> request = Set.of(1L);
        shipmentDao.findShipmentsByIds(request);
        verify(shipmentRepository, times(1)).findShipmentsByIds(request);
    }

    @Test
    void delete() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDao.delete(shipmentDetails);
        verify(shipmentRepository, times(1)).delete(any());
    }

    @Test
    void findByGuid() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        when(shipmentRepository.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        assertEquals(Optional.of(shipmentDetails), shipmentDao.findByGuid(UUID.randomUUID()));
    }

    @Test
    void findByGuids() {
        // Arrange
        List<UUID> guids = List.of(UUID.randomUUID(), UUID.randomUUID());
        List<ShipmentDetails> shipmentDetailsList = List.of(
                ShipmentDetails.builder().build(),
                ShipmentDetails.builder().build()
        );
        when(shipmentRepository.findAllByGuids(anyList())).thenReturn(shipmentDetailsList);

        // Act
        List<ShipmentDetails> result = shipmentDao.findByGuids(guids);

        // Assert
        assertEquals(shipmentDetailsList, result);
        verify(shipmentRepository, times(1)).findAllByGuids(guids); // Ensures the method was called with the correct argument
    }

    @Test
    void findByBookingReference() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        List<ShipmentDetails> shipmentDetailsList = Arrays.asList(shipmentDetails);
        when(shipmentRepository.findByBookingReference(any(), any())).thenReturn(shipmentDetailsList);
        assertEquals(shipmentDetailsList, shipmentDao.findByBookingReference("ref", 1));
    }

    @Test
    void findMaxId() {
        when(shipmentRepository.findMaxId()).thenReturn(1L);
        assertEquals(1L, shipmentDao.findMaxId());
    }

    @Test
    void applyShipmentValidationsFindByHBLTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_IMP)
                .houseBill("HBL123")
                .build();

        when(shipmentRepository.findByHouseBill(any(), any())).thenReturn(Collections.singletonList(ShipmentDetails.builder().build()));
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsFindByHBLCancelledStatusTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_IMP)
                .houseBill("HBL123")
                .status(3)
                .bookingReference("bref123")
                .build();


        when(shipmentRepository.findByHouseBill(any(), any())).thenReturn(Collections.singletonList(ShipmentDetails.builder().build()));
        when(shipmentRepository.findByBookingReference(any(), any())).thenReturn(Collections.singletonList(ShipmentDetails.builder().build()));
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsDifferentConsolTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ConsolidationDetails consolidationDetails2 = ConsolidationDetails.builder().build();
        consolidationDetails.setId(2L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_IMP)
                .houseBill("HBL123")
                .status(3)
                .bookingReference("bref123")
                .masterBill("Mast77777770")
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails2);

        when(shipmentRepository.findByHouseBill(any(), any())).thenReturn(Arrays.asList(ShipmentDetails.builder().build()));
        when(shipmentRepository.findByBookingReference(any(), any())).thenReturn(Arrays.asList(ShipmentDetails.builder().build()));

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void saveAll() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(Containers.builder().build())));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setId(1L);

        List<ShipmentDetails> shipmentDetailsList = Arrays.asList(shipmentDetails);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetailsList, shipmentDao.saveAll(shipmentDetailsList));
    }

    @Test
    void saveTestContainerConsolidationNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void saveTestIdNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void testSaveWithCancelledShipment() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setId(1L);
        shipmentDetails.setStatus(ShipmentStatus.Cancelled.getValue());
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        mockShipmentSettings();
        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }

    @Test
    void findAllTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

        when(shipmentRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        assertEquals(shipmentDetailsPage, shipmentDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void entityDetach() {
        shipmentDao.entityDetach(List.of(ShipmentDetails.builder().build()));
        verify(entityManager).detach(any());
    }

    @Test
    void findBySourceGuid() {
        when(shipmentRepository.findBySourceGuid(any())).thenReturn(List.of(new ShipmentDetails()));
        var response = shipmentDao.findBySourceGuid(UUID.randomUUID());
        assertFalse(response.isEmpty());
    }

    @Test
    void testGetIdWithPendingActions() {
        List<Long> eligibleShipmentId = List.of(1L, 2L, 3L);

        Page<Long> shipmentIdPage = new PageImpl<>(eligibleShipmentId);
        when(shipmentRepository.getIdWithPendingActions(any(), any())).thenReturn(shipmentIdPage);
        var response = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED, PageRequest.of(1, 25));

        assertEquals(shipmentIdPage, response);
    }

    @Test
    void findAllWithoutTenantCheckTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

        when(shipmentRepository.findAllWithoutTenantFilter(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        assertEquals(shipmentDetailsPage, shipmentDao.findAllWithoutTenantFilter(pair.getLeft(), pair.getRight()));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_HazShipment1() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentType(Constants.SHIPMENT_TYPE_LCL)
                .jobType(Constants.SHIPMENT_TYPE_STD)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(true)
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_HazShipment2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentType(Constants.CARGO_TYPE_FCL)
                .jobType(Constants.SHIPMENT_TYPE_STD)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(true)
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_HazShipment3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentType(Constants.SHIPMENT_TYPE_LCL)
                .jobType(Constants.CONSOLIDATION_TYPE_AGT)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(true)
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_HazShipment4() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        Parties parties = Parties.builder().type("type").build();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build(), ConsolidationDetails.builder().build())))
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentType(Constants.SHIPMENT_TYPE_LCL)
                .jobType(Constants.CONSOLIDATION_TYPE_CLD)
                .packingList(Arrays.asList(packing))
                .routingsList(Arrays.asList(routings, routings))
                .containersList(new HashSet<>(Arrays.asList(containers, containers)))
                .shipmentAddresses(Arrays.asList(parties, parties))
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails, consolidationDetails)))
                .carrierDetails(CarrierDetails.builder().build())
                .direction(Constants.DIRECTION_EXP)
                .masterBill("MBL123")
                .containsHazardous(true)
                .build();

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.AIR_DG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
    }

    @Test
    void findShipmentByIdWithQuery() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);
        when(shipmentRepository.findShipmentByIdWithQuery(anyLong())).thenReturn(Optional.of(shipmentDetails));
        Optional<ShipmentDetails> responseEntity = shipmentDao.findShipmentByIdWithQuery(shipmentDetails.getId());
        assertEquals(Optional.of(shipmentDetails), responseEntity);
    }

    @Test
    void updateFCRNo() {
        assertDoesNotThrow(() -> shipmentDao.updateFCRNo(1L));
    }

    @Test
    void findReceivingByGuid() {
        var guid = UUID.randomUUID();
        when(shipmentRepository.findReceivingByGuid(guid)).thenReturn(1);
        var res = shipmentDao.findReceivingByGuid(guid);
        assertEquals(1, res);
    }

    @Test
    void applyShipmentValidationsTest_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(true);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .direction(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
        assertTrue(errors.contains("The shipment contains DG package. Marking the shipment as non DG is not allowed"));
    }

    @Test
    void applyShipmentValidationsExpTest_CountryAirCargoSecurity() {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isAirSecurityLicense).thenReturn(false);
            ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder().countryAirCargoSecurity(true)
                    .restrictedLocationsEnabled(true).build());

            Packing packing = new Packing();
            packing.setHazardous(true);
            UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());

            ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .direction(Constants.DIRECTION_EXP)
                .build();

            mockShipmentSettings();
            Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
            assertTrue(errors.contains(
                "You don't have Air Security permission to create or update AIR EXP Shipment."));
        }
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_CountryAirCargoSecurity() {
        try (MockedStatic<LicenseContext> mockedLicenseContext = mockStatic(LicenseContext.class)) {
            mockedLicenseContext.when(LicenseContext::isAirSecurityLicense).thenReturn(true);
            ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder().countryAirCargoSecurity(true)
                    .restrictedLocationsEnabled(true).build());

            Packing packing = new Packing();
            packing.setHazardous(false);
            Map<String, Boolean> permissions = new HashMap<>();
            permissions.put(PermissionConstants.AIR_SECURITY_PERMISSION, true);
            UserContext.setUser(UsersDto.builder().Permissions(permissions).build());

            ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .containsHazardous(false)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .direction(Constants.DIRECTION_EXP)
                .build();

            mockShipmentSettings();
            Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, false);
            assertFalse(errors.contains(
                "You don't have Air Security permission to create or update AIR EXP Shipment."));
        }
    }

    @Test
    void applyShipmentValidationsTest_NonHazPack_CountryAirCargoSecurity_V1Sync() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .containsHazardous(true)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(Arrays.asList(packing))
                .direction(Constants.DIRECTION_IMP)
                .build();

        mockShipmentSettings();
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, true);
        assertFalse(errors.contains("You don't have Air Security permission to create or update AIR EXP Shipment."));
    }
}
