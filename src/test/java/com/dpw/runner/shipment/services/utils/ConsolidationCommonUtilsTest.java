package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ConsolidationCommonUtilsTest extends CommonMocks {

    @InjectMocks
    private ConsolidationCommonUtils consolidationCommonUtils;

    @Mock
    private IV1Service v1Service;

    private static ShipmentSettingsDetails shipmentSettingsDetails;

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").enableRouteMaster(true).build());
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().build());
        shipmentSettingsDetails = new ShipmentSettingsDetails();
    }

    @Test
    void testGenerateCustomBolNumber_Success_Random() {
        shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setBolNumberPrefix("CONS");
        shipmentSettingsDetails.setBolNumberGeneration(GenerationType.Random);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        mockShipmentSettings();
        String res = consolidationCommonUtils.generateCustomBolNumber();
        assertEquals(14, res.length());
    }

    @Test
    void shouldSetReceivingBranchToNullWhenZero() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setReceivingBranch(0L);

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getReceivingBranch()).isNull();
    }

    @Test
    void shouldKeepReceivingBranchWhenNonZero() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setReceivingBranch(10L);

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getReceivingBranch()).isEqualTo(10L);
    }

    @Test
    void shouldNullifyTriangulationPartnerListIfOnlyOneWithZero() {
        TriangulationPartner partner = new TriangulationPartner();
        partner.setTriangulationPartner(0L);
        ConsolidationDetails details = new ConsolidationDetails();
        details.setTriangulationPartnerList(List.of(partner));

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getTriangulationPartnerList()).isNull();
    }

    @Test
    void shouldNotNullifyTriangulationPartnerListIfOneWithNonZero() {
        TriangulationPartner partner = new TriangulationPartner();
        partner.setTriangulationPartner(123L);
        ConsolidationDetails details = new ConsolidationDetails();
        details.setTriangulationPartnerList(List.of(partner));

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getTriangulationPartnerList()).isNotNull();
        assertThat(details.getTriangulationPartnerList()).hasSize(1);
    }

    @Test
    void shouldSetTriangulationPartnerToNullWhenListIsNullAndValueIsZero() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setTriangulationPartnerList(null);
        details.setTriangulationPartner(0L);

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getTriangulationPartner()).isNull();
    }

    @Test
    void shouldNotSetTriangulationPartnerToNullWhenListIsNullAndValueIsNonZero() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setTriangulationPartnerList(null);
        details.setTriangulationPartner(999L);

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getTriangulationPartner()).isEqualTo(999L);
    }

    @Test
    void shouldDoNothingWhenAllInputsAreValid() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setReceivingBranch(100L);
        details.setTriangulationPartner(500L);
        details.setTriangulationPartnerList(Arrays.asList(
                new TriangulationPartner(101L, true),
                new TriangulationPartner(102L, false)
        ));

        consolidationCommonUtils.setReceivingAndTriangulationBranch(details);

        assertThat(details.getReceivingBranch()).isEqualTo(100L);
        assertThat(details.getTriangulationPartner()).isEqualTo(500L);
        assertThat(details.getTriangulationPartnerList()).hasSize(2);
    }

    @Test
    void testGenerateCustomBolNumber_Success_Serial() {
        shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setBolNumberPrefix("CONS");
        shipmentSettingsDetails.setBolNumberGeneration(GenerationType.Serial);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        when(v1Service.getMaxConsolidationId()).thenReturn("2313");
        mockShipmentSettings();

        String res = consolidationCommonUtils.generateCustomBolNumber();
        assertEquals("CONS2313", res);
    }

}
