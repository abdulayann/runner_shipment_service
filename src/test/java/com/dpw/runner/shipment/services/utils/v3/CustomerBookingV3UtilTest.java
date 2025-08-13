package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.service.impl.ConsolidationV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.times;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class CustomerBookingV3UtilTest {

    @Mock
    private IContainerDao containerDao;

    @Mock
    private ICustomerBookingDao customerBookingDao;

    @Mock
    private IPackingDao packingDao;

    @Mock
    private ConsolidationV3Service consolidationService;

    @Mock
    private CommonUtils commonUtils;

    @InjectMocks
    private CustomerBookingV3Util customerBookingV3Util;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static CustomerBooking customerBooking;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        MDC.setContextMap(new HashMap<>());
        customerBooking = jsonTestUtility.getCustomerBooking();
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).FetchRatesMandate(Boolean.FALSE).ShipmentServiceV2Enabled(Boolean.TRUE).countryAirCargoSecurity(Boolean.TRUE).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().hasNoUtilization(true).isAlwaysUtilization(Boolean.TRUE).build());
    }

    @Test
    void testUpdateCargoInfoInBookingWithContainers() throws RunnerException {
        Containers containers = new Containers();
        containers.setContainerCode("20FR");
        containers.setContainerCount(1L);
        containers.setBookingId(2L);
        containers.setId(3L);
        containers.setPackagesPerContainer(1L);
        containers.setCargoWeightPerContainer(BigDecimal.TEN);
        containers.setContainerPackageType("BAG");
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        Map<String, BigDecimal> teuMap = new HashMap<>();
        teuMap.put("20FR", BigDecimal.ONE);
        when(containerDao.findByBookingIdIn(anyList())).thenReturn(List.of(containers));
        when(packingDao.findByBookingIdIn(anyList())).thenReturn(List.of());
        when(consolidationService.determineWeightChargeableUnit(any())).thenReturn("KG");
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.ONE);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        customerBookingV3Util.updateCargoInformation(customerBooking, teuMap, null, false);
        verify(customerBookingDao, times(1)).save(any(CustomerBooking.class));
    }

    @Test
    void testUpdateCargoInfoInBookingWithPackages() throws RunnerException {
        Packing packing = new Packing();
        packing.setBookingId(1L);
        packing.setVolume(BigDecimal.TEN);
        packing.setVolumeUnit("M3");
        packing.setWeight(BigDecimal.TEN);
        packing.setWeightUnit("KG");
        packing.setPacks("2");
        packing.setPacksType("BAG");
        Packing packing1 = new Packing();
        packing1.setBookingId(2L);
        packing1.setVolume(BigDecimal.ONE);
        packing1.setVolumeUnit("M3");
        packing1.setWeightUnit("KG");
        packing1.setPacksType("BAG");
        packing1.setPacks("3");
        Map<String, BigDecimal> teuMap = new HashMap<>();
        teuMap.put("20FR", BigDecimal.ONE);
        when(containerDao.findByBookingIdIn(anyList())).thenReturn(List.of());
        when(packingDao.findByBookingIdIn(anyList())).thenReturn(List.of(packing, packing1));
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.ONE);
        when(consolidationService.determineWeightChargeableUnit(any())).thenReturn("KG");
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        customerBookingV3Util.updateCargoInformation(customerBooking, teuMap, null, false);
        verify(customerBookingDao, times(1)).save(any(CustomerBooking.class));
    }
}
