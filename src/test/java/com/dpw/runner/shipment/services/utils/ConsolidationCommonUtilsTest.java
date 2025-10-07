package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ConsolidationCommonUtilsTest extends CommonMocks {

    @InjectMocks
    private ConsolidationCommonUtils consolidationCommonUtils;

    @Mock
    private IV1Service v1Service;

    @Mock
    private IEventDao eventDao;

    @Mock
    private ProductIdentifierUtility productEngine;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private GetNextNumberHelper getNextNumberHelper;

    private static ShipmentSettingsDetails shipmentSettingsDetails;
    private static ConsolidationDetails testConsol;
    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetailsResponse testConsolResponse;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").enableRouteMaster(true).build());
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().build());
        shipmentSettingsDetails = new ShipmentSettingsDetails();
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        testConsol.setAssignedTo("AssignedToUser");
        testConsolResponse = objectMapperTest.convertValue(testConsol , ConsolidationDetailsResponse.class);
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

    @Test
    void testGenerateConsolidationNumberWithConsolidationLiteTrue() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(true);
        when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(false);
        when(v1Service.getMaxConsolidationId()).thenReturn("123311");
        mockShipmentSettings();
        consolidationCommonUtils.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
        assertNull(consolidationDetails.getBol());
    }

    @Test
    void testGenerateConsolidationNumber_Success() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(false);
        when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(true);
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(tenantProducts));
        when(productEngine.getCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI)).thenReturn("CONS007262");
        when(productEngine.identifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
        when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
        when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("BOL23131");
        mockShipmentSettings();
        consolidationCommonUtils.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS007262", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS007262", consolidationDetails.getReferenceNumber());
        assertEquals("BOL23131", consolidationDetails.getBol());
    }

    @Test
    void testGenerateConsolidationNumber_Success1() throws RunnerException {
        var spyService = Mockito.spy(consolidationCommonUtils);
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(false);
        when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(true);
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(tenantProducts));
        when(productEngine.getCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI)).thenReturn("");
        when(productEngine.identifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
        when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
        when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("");
        when(v1Service.getMaxConsolidationId()).thenReturn("123311");
        doReturn("BOL2121").when(spyService).generateCustomBolNumber();
        mockShipmentSettings();
        spyService.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
        assertEquals("BOL2121", consolidationDetails.getBol());
    }

    @Test
    void testGenerateConsolidationNumber_Success2() throws RunnerException {
        var spyService = Mockito.spy(consolidationCommonUtils);
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(false);
        when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(false);
        when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(tenantProducts));
        when(productEngine.identifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
        when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
        when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("");
        when(v1Service.getMaxConsolidationId()).thenReturn("123311");
        doReturn("BOL2121").when(spyService).generateCustomBolNumber();
        mockShipmentSettings();
        spyService.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
        assertEquals("BOL2121", consolidationDetails.getBol());
    }

    @Test
    void testCreateEvent_Success() {
        consolidationCommonUtils.createEvent(testConsol, "EventCode");
        verify(eventDao, times(1)).save(any(Events.class));
    }

}
