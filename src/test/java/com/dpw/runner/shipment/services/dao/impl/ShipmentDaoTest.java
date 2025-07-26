package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
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
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
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
import javax.validation.ConstraintViolationException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private IPackingDao packingDao;

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
        Containers containers = Containers.builder().build();
        containers.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(containers)));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertThrows(ValidationException.class, () -> {
            shipmentDao.save(shipmentDetails, false);
        });
    }

    @Test
    void saveTestOldEntityNotPresent() {
        Containers containers = Containers.builder().build();
        containers.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(containers)));
        shipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(ConsolidationDetails.builder().build())));
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentDao.save(shipmentDetails, false);
        });
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
        Containers containers = Containers.builder().build();
        containers.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(containers)));
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
        Containers containers = Containers.builder().build();
        containers.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(containers)));
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        containers.setGuid(UUID.randomUUID());
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
    void applyShipmentValidationsExpTest_NonHazPack_NonDgUser() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).restrictedLocationsEnabled(true).build());

        Packing packing = new Packing();
        packing.setHazardous(false);

        Routings routings = new Routings();
        routings.setLeg(1L);

        Containers containers = Containers.builder().containerNumber("CON123").build();
        containers.setGuid(UUID.randomUUID());
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
        assertFalse(errors.contains("Container Number cannot be same for two different containers"));
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
                .carrierDetails(CarrierDetails.builder().origin("origin").originPort("origin").destination("destination").destinationPort("destination").build())
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
                .carrierDetails(CarrierDetails.builder().origin("origin").originPort("origin").destination("destination").destinationPort("destination").etd(LocalDateTime.now()).etd(LocalDateTime.now()).build())
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
                        .destination("destination")
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
        assertThrows(ValidationException.class, () -> {
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
                        .destination("destination")
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
                        .destination("destination")
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
    void updateMawbCheckTest_Exception1() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).isRunnerV3Enabled(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("destination")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));

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
        assertThrows(DataRetrievalFailureException.class,() -> shipmentDao.update(shipmentDetails, false));
    }

    @Test
    void updateMawbCheckTest_Exception2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).isRunnerV3Enabled(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBorrowedFrom(Parties.builder().orgCode("org1").build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("destination")
                        .destinationPort("destination")
                        .etd(LocalDateTime.now())
                        .etd(LocalDateTime.now())
                        .build())
                .jobType("DRT")
                .direction("EXP")
                .partner("CLD")
                .additionalDetails(additionalDetails)
                .build();

        shipmentDetails.setId(1L);
        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));

        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        consolidationDetailsList.add(consolidationDetails);

        when(consolidationDetailsDao.findByBol(any())).thenReturn(consolidationDetailsList);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().status(Constants.UNUSED).parentId(2L).build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());
        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(MawbStocks.builder().borrowedFrom("org2").build()));
        mockShipmentSettings();
        assertThrows(ValidationException.class,() -> shipmentDao.update(shipmentDetails, false));
    }

    @Test
    void updateMawbCheckTest1() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).isRunnerV3Enabled(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        AdditionalDetails additionalDetails = new AdditionalDetails();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("destination")
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

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().status(Constants.UNUSED).parentId(1L).build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());
        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(MawbStocks.builder().borrowedFrom("org2").build()));
        when(v1ServiceUtil.getOrganizationDataFromV1("org2")).thenReturn(Parties.builder().build());
        mockShipmentSettings();
        ShipmentDetails response = shipmentDao.update(shipmentDetails, false);
        assertNotNull(response);
    }

    @Test
    void updateMawbCheckTest2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).isRunnerV3Enabled(true).cancelledBLSuffix("BL").build());

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        AdditionalDetails additionalDetails = new AdditionalDetails();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .houseBill("HBL123")
                .masterBill("Mast77777770")
                .status(3)
                .carrierDetails(CarrierDetails.builder()
                        .origin("origin")
                        .originPort("origin")
                        .destination("destination")
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

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().status(Constants.UNUSED).parentId(1L).build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(CarrierResponse.builder().iATACode("iATA").build()));
        when(v1Service.fetchCarrierMasterData(any(), eq(false))).thenReturn(V1DataResponse.builder().build());
        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(MawbStocks.builder().build()));
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
                        .destination("destination")
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        Containers containers = Containers.builder().build();
        containers.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(new HashSet<>(Collections.singletonList(containers)));
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        containers.setGuid(UUID.randomUUID());
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
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

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
        assertTrue(errors.contains("You don't have Air Security permission to create or update AIR EXP Shipment."));
    }

    @Test
    void applyShipmentValidationsExpTest_NonHazPack_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().countryAirCargoSecurity(true).restrictedLocationsEnabled(true).build());

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
        assertFalse(errors.contains("You don't have Air Security permission to create or update AIR EXP Shipment."));
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

    @Test
    void testUpdateShipmentsBookingNumber() {
        when(shipmentRepository.updateShipmentsBookingNumber(Mockito.<List<UUID>>any(), Mockito.<String>any()))
                .thenReturn(10);
        int actualUpdateShipmentsBookingNumberResult = shipmentDao.updateShipmentsBookingNumber(new ArrayList<>(), "42");
        verify(shipmentRepository).updateShipmentsBookingNumber(Mockito.<List<UUID>>any(), Mockito.<String>any());
        assertEquals(10, actualUpdateShipmentsBookingNumberResult);
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateCargoDetailsInShipment(Long, Integer, String, BigDecimal, String, BigDecimal, String, BigDecimal, String, BigDecimal, String)}
     */
    @Test
    void testUpdateCargoDetailsInShipment() {
        when(shipmentRepository.updateCargoDetailsInShipment(Mockito.<Long>any(), Mockito.<Integer>any(),
                Mockito.<String>any(), Mockito.<BigDecimal>any(), Mockito.<String>any(), Mockito.<BigDecimal>any(),
                Mockito.<String>any(), Mockito.<BigDecimal>any(), Mockito.<String>any(), Mockito.<BigDecimal>any(),
                Mockito.<String>any())).thenReturn(1);
        BigDecimal volume = new BigDecimal("2.3");
        BigDecimal weight = new BigDecimal("2.3");
        BigDecimal volumetricWeight = new BigDecimal("2.3");
        shipmentDao.updateCargoDetailsInShipment(1L, 1, "Packs Unit", volume, "Volume Unit", weight, "Weight Unit",
                volumetricWeight, "Volumetric Weight Unit", new BigDecimal("2.3"), "Chargeable Unit");
        verify(shipmentRepository).updateCargoDetailsInShipment(Mockito.<Long>any(), Mockito.<Integer>any(),
                Mockito.<String>any(), Mockito.<BigDecimal>any(), Mockito.<String>any(), Mockito.<BigDecimal>any(),
                Mockito.<String>any(), Mockito.<BigDecimal>any(), Mockito.<String>any(), Mockito.<BigDecimal>any(),
                Mockito.<String>any());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateShipmentDetailsFromPacks(Long, DateBehaviorType, LocalDateTime, ShipmentPackStatus)}
     */
    @Test
    void testUpdateShipmentDetailsFromPacks() {
        doNothing().when(shipmentRepository)
                .updateShipmentDetailsFromPacks(Mockito.<Long>any(), Mockito.<DateBehaviorType>any(),
                        Mockito.<LocalDateTime>any(), Mockito.<ShipmentPackStatus>any());
        shipmentDao.updateShipmentDetailsFromPacks(1L, DateBehaviorType.ACTUAL, LocalDate.of(1970, 1, 1).atStartOfDay(),
                ShipmentPackStatus.BOOKED);
        verify(shipmentRepository).updateShipmentDetailsFromPacks(Mockito.<Long>any(), Mockito.<DateBehaviorType>any(),
                Mockito.<LocalDateTime>any(), Mockito.<ShipmentPackStatus>any());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateShipmentDetailsFromPacks(Long, DateBehaviorType, LocalDateTime, ShipmentPackStatus)}
     */
    @Test
    void testUpdateShipmentDetailsFromPacks2() {
        doNothing().when(shipmentRepository)
                .updateShipmentDetailsFromPacks(Mockito.<Long>any(), Mockito.<DateBehaviorType>any(),
                        Mockito.<LocalDateTime>any(), Mockito.<ShipmentPackStatus>any());
        shipmentDao.updateShipmentDetailsFromPacks(1L, DateBehaviorType.ESTIMATED, LocalDate.of(1970, 1, 1).atStartOfDay(),
                ShipmentPackStatus.BOOKED);
        verify(shipmentRepository).updateShipmentDetailsFromPacks(Mockito.<Long>any(), Mockito.<DateBehaviorType>any(),
                Mockito.<LocalDateTime>any(), Mockito.<ShipmentPackStatus>any());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
    }

    /**
     * Method under test: {@link ShipmentDao#setShipmentIdsToContainer(List, Long)}
     */
    @Test
    void testSetShipmentIdsToContainer() {
        doNothing().when(shipmentRepository).setShipmentIdsToContainer(Mockito.<List<Long>>any(), Mockito.<Long>any());
        shipmentDao.setShipmentIdsToContainer(new ArrayList<>(), 1L);
        verify(shipmentRepository).setShipmentIdsToContainer(Mockito.<List<Long>>any(), Mockito.<Long>any());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
    }

    /**
     * Method under test: {@link ShipmentDao#setShipmentIdsToContainer(List, Long)}
     */
    @Test
    void testSetShipmentIdsToContainer2() {
        doNothing().when(shipmentRepository).setShipmentIdsToContainer(Mockito.<List<Long>>any(), Mockito.<Long>any());

        ArrayList<Long> shipmentIds = new ArrayList<>();
        shipmentIds.add(1L);
        shipmentDao.setShipmentIdsToContainer(shipmentIds, 1L);
        verify(shipmentRepository).setShipmentIdsToContainer(Mockito.<List<Long>>any(), Mockito.<Long>any());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
    }

    /**
     * Method under test: {@link ShipmentDao#setShipmentIdsToContainer(List, Long)}
     */
    @Test
    void testSetShipmentIdsToContainer3() {
        doNothing().when(shipmentRepository).setShipmentIdsToContainer(Mockito.<List<Long>>any(), Mockito.<Long>any());

        ArrayList<Long> shipmentIds = new ArrayList<>();
        shipmentIds.add(0L);
        shipmentIds.add(1L);
        shipmentDao.setShipmentIdsToContainer(shipmentIds, 1L);
        verify(shipmentRepository).setShipmentIdsToContainer(Mockito.<List<Long>>any(), Mockito.<Long>any());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateSailingScheduleRelatedInfo(ShipmentSailingScheduleRequest, Long)}
     */
    @Test
    void testUpdateSailingScheduleRelatedInfo() {
        doNothing().when(shipmentRepository)
                .updateSailingScheduleRelatedInfo(Mockito.<Long>any(), Mockito.<LocalDateTime>any(),
                        Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                        Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                        Mockito.<LocalDateTime>any());

        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        request.setCarrier("Carrier");
        request.setDgCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestDropOffFullEquipmentToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestEmptyEquipmentPickUp(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestArrivalTime(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestFullEquipmentDeliveredToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setReeferCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setRoutings(new ArrayList<>());
        request.setShippingInstructionCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setTerminalCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setVerifiedGrossMassCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        shipmentDao.updateSailingScheduleRelatedInfo(request, 1L);
        verify(shipmentRepository).updateSailingScheduleRelatedInfo(Mockito.<Long>any(), Mockito.<LocalDateTime>any(),
                Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                Mockito.<LocalDateTime>any());
        assertEquals("00:00", request.getDgCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getEarliestDropOffFullEquipmentToCarrier().toLocalTime().toString());
        assertEquals("00:00", request.getEarliestEmptyEquipmentPickUp().toLocalTime().toString());
        assertEquals("00:00", request.getLatestArrivalTime().toLocalTime().toString());
        assertEquals("00:00", request.getLatestFullEquipmentDeliveredToCarrier().toLocalTime().toString());
        assertEquals("00:00", request.getReeferCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getShippingInstructionCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getTerminalCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getVerifiedGrossMassCutoff().toLocalTime().toString());
        assertEquals("Carrier", request.getCarrier());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
        assertTrue(request.getRoutings().isEmpty());
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateSailingScheduleRelatedInfo(ShipmentSailingScheduleRequest, Long)}
     */
    @Test
    void testUpdateSailingScheduleRelatedInfo2() {
        doThrow(new ConstraintViolationException(new HashSet<>())).when(shipmentRepository)
                .updateSailingScheduleRelatedInfo(Mockito.<Long>any(), Mockito.<LocalDateTime>any(),
                        Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                        Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                        Mockito.<LocalDateTime>any());

        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        request.setCarrier("Carrier");
        request.setDgCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestDropOffFullEquipmentToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestEmptyEquipmentPickUp(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestArrivalTime(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestFullEquipmentDeliveredToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setReeferCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setRoutings(new ArrayList<>());
        request.setShippingInstructionCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setTerminalCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setVerifiedGrossMassCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        assertThrows(ConstraintViolationException.class, () -> shipmentDao.updateSailingScheduleRelatedInfo(request, 1L));
        verify(shipmentRepository).updateSailingScheduleRelatedInfo(Mockito.<Long>any(), Mockito.<LocalDateTime>any(),
                Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(), Mockito.<LocalDateTime>any(),
                Mockito.<LocalDateTime>any());
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateSailingScheduleRelatedInfoForAir(ShipmentSailingScheduleRequest, Long)}
     */
    @Test
    void testUpdateSailingScheduleRelatedInfoForAir() {
        doNothing().when(shipmentRepository)
                .updateSailingScheduleRelatedInfoForAir(Mockito.<Long>any(), Mockito.<LocalDateTime>any());

        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        request.setCarrier("Carrier");
        request.setDgCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestDropOffFullEquipmentToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestEmptyEquipmentPickUp(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestArrivalTime(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestFullEquipmentDeliveredToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setReeferCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setRoutings(new ArrayList<>());
        request.setShippingInstructionCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setTerminalCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setVerifiedGrossMassCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        shipmentDao.updateSailingScheduleRelatedInfoForAir(request, 1L);
        verify(shipmentRepository).updateSailingScheduleRelatedInfoForAir(Mockito.<Long>any(),
                Mockito.<LocalDateTime>any());
        assertEquals("00:00", request.getDgCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getEarliestDropOffFullEquipmentToCarrier().toLocalTime().toString());
        assertEquals("00:00", request.getEarliestEmptyEquipmentPickUp().toLocalTime().toString());
        assertEquals("00:00", request.getLatestArrivalTime().toLocalTime().toString());
        assertEquals("00:00", request.getLatestFullEquipmentDeliveredToCarrier().toLocalTime().toString());
        assertEquals("00:00", request.getReeferCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getShippingInstructionCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getTerminalCutoff().toLocalTime().toString());
        assertEquals("00:00", request.getVerifiedGrossMassCutoff().toLocalTime().toString());
        assertEquals("Carrier", request.getCarrier());
        assertEquals(0L, shipmentDao.findMaxId().longValue());
        assertTrue(request.getRoutings().isEmpty());
    }

    /**
     * Method under test:
     * {@link ShipmentDao#updateSailingScheduleRelatedInfoForAir(ShipmentSailingScheduleRequest, Long)}
     */
    @Test
    void testUpdateSailingScheduleRelatedInfoForAir2() {
        doThrow(new ConstraintViolationException(new HashSet<>())).when(shipmentRepository)
                .updateSailingScheduleRelatedInfoForAir(Mockito.<Long>any(), Mockito.<LocalDateTime>any());

        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        request.setCarrier("Carrier");
        request.setDgCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestDropOffFullEquipmentToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setEarliestEmptyEquipmentPickUp(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestArrivalTime(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setLatestFullEquipmentDeliveredToCarrier(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setReeferCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setRoutings(new ArrayList<>());
        request.setShippingInstructionCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setTerminalCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        request.setVerifiedGrossMassCutoff(LocalDate.of(1970, 1, 1).atStartOfDay());
        assertThrows(ConstraintViolationException.class,
                () -> shipmentDao.updateSailingScheduleRelatedInfoForAir(request, 1L));
        verify(shipmentRepository).updateSailingScheduleRelatedInfoForAir(Mockito.<Long>any(),
                Mockito.<LocalDateTime>any());
    }

    @Test
    void findByShipmentIdInAndContainsHazardousTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        when(shipmentRepository.findByShipmentIdInAndContainsHazardous(any(), anyBoolean())).thenReturn(shipmentDetailsList);
        List<ShipmentDetails> shipmentDetails1 = shipmentDao.findByShipmentIdInAndContainsHazardous(List.of(1L), false);
        assertNotNull(shipmentDetails1);
    }

    @Test
    void findByShipmentIdInTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        when(shipmentRepository.findByShipmentIdIn(any())).thenReturn(shipmentDetailsList);
        List<ShipmentDetails> shipmentDetails1 = shipmentDao.findByShipmentIdIn(List.of("SHP0001"));
        assertNotNull(shipmentDetails1);
    }

    @Test
    void findShipmentByGuidWithQueryTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);

        when(shipmentRepository.findShipmentByGuidWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findShipmentByGuidWithQuery(UUID.randomUUID());
        assertNotNull(shipmentDetails1);
    }

    @Test
    void testSaveIsTransferredToReceivingBranchTest() {
        Long shipmentId = 123L;
        Boolean isTransferred = true;

        shipmentDao.saveIsTransferredToReceivingBranch(shipmentId, isTransferred);

        verify(shipmentRepository, times(1))
                .saveIsTransferredToReceivingBranch(shipmentId, isTransferred);
    }

    @Test
    void updateIsAcceptedTriangulationPartnerTest() {
        Long shipmentId = 123L;
        Long triangulationPartner = 234L;
        Boolean isTransferred = true;

        shipmentDao.updateIsAcceptedTriangulationPartner(shipmentId, triangulationPartner, isTransferred);

        verify(shipmentRepository, times(1))
                .updateIsAcceptedTriangulationPartner(shipmentId, triangulationPartner, isTransferred);
    }

    @Test
    void saveWithoutValidationTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);

        when(shipmentRepository.save(shipmentDetails)).thenReturn(shipmentDetails);
        ShipmentDetails shipmentDetails1 = shipmentDao.saveWithoutValidation(shipmentDetails);
        assertNotNull(shipmentDetails1);

    }

    @Test
    void updateAdditionalDetailsByShipmentIdTest() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1L);

        shipmentDao.updateAdditionalDetailsByShipmentId(1l, true);
        verify(shipmentRepository, times(1))
                .updateAdditionalDetailsByShipmentId(1l, true);
    }

    @Test
    void updateDgStatusInShipmentTest(){
        Boolean isHazardous = true;
        String oceanDGStatus = "oceanDGStatus";
        Long id = 1L;

        shipmentDao.updateDgStatusInShipment(isHazardous, oceanDGStatus, id);
        verify(shipmentRepository, times(1))
                .updateDgStatusInShipment(isHazardous, oceanDGStatus, id);
    }
}
