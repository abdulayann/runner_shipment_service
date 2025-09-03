package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionValidationUtil.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class TransportInstructionValidationUtilTest {
    @MockBean
    private CommonUtils commonUtils;

    @MockBean
    private IShipmentServiceV3 iShipmentServiceV3;

    @MockBean
    private MasterDataKeyUtils masterDataKeyUtils;

    @MockBean
    private MasterDataUtils masterDataUtils;

    @Autowired
    private TransportInstructionValidationUtil transportInstructionValidationUtil;

    @Test
    void testAddAllTiLegsMasterDataInSingleCallList() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
        ArrayList<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiLegsMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiLegsMasterDataInSingleCallList2() {
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any()))
                .thenThrow(new ValidationException("ItemType"));
        ArrayList<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiLegsMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
    }

    @Test
    void testAddAllTiLegsMasterDataInSingleCallList3() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        TransportInstructionLegsResponse transportInstructionLegsResponse = new TransportInstructionLegsResponse();
        transportInstructionLegsResponse.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        PartiesResponse.PartiesResponseBuilder entityTypeResult = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult = entityTypeResult.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult = orgCodeResult.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse destination = orgIdResult.rAKCDetails(rAKCDetails).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setDestination(destination);
        transportInstructionLegsResponse.setDropMode("ItemType");
        transportInstructionLegsResponse.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setGuid(UUID.randomUUID());
        transportInstructionLegsResponse.setId(1L);
        transportInstructionLegsResponse.setLegType("ItemType");
        PartiesResponse.PartiesResponseBuilder entityTypeResult2 = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult2 = entityTypeResult2.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult2 = orgCodeResult2.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails2 = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse origin = orgIdResult2.rAKCDetails(rAKCDetails2).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setOrigin(origin);
        transportInstructionLegsResponse.setRemarks("ItemType");
        transportInstructionLegsResponse.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setSequence(5L);

        ArrayList<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        responseList.add(transportInstructionLegsResponse);
        transportInstructionValidationUtil.addAllTiLegsMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiLegsMasterDataInSingleCallList4() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        TransportInstructionLegsResponse transportInstructionLegsResponse = new TransportInstructionLegsResponse();
        transportInstructionLegsResponse.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        PartiesResponse.PartiesResponseBuilder entityTypeResult = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult = entityTypeResult.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult = orgCodeResult.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse destination = orgIdResult.rAKCDetails(rAKCDetails).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setDestination(destination);
        transportInstructionLegsResponse.setDropMode("ItemType");
        transportInstructionLegsResponse.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setGuid(UUID.randomUUID());
        transportInstructionLegsResponse.setId(1L);
        transportInstructionLegsResponse.setLegType("ItemType");
        PartiesResponse.PartiesResponseBuilder entityTypeResult2 = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult2 = entityTypeResult2.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult2 = orgCodeResult2.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails2 = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse origin = orgIdResult2.rAKCDetails(rAKCDetails2).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setOrigin(origin);
        transportInstructionLegsResponse.setRemarks("ItemType");
        transportInstructionLegsResponse.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setSequence(5L);

        ArrayList<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        responseList.add(transportInstructionLegsResponse);
        transportInstructionValidationUtil.addAllTiLegsMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiLegsMasterDataInSingleCallList5() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        MasterListRequest buildResult2 = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult2);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        TransportInstructionLegsResponse transportInstructionLegsResponse = new TransportInstructionLegsResponse();
        transportInstructionLegsResponse.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        PartiesResponse.PartiesResponseBuilder entityTypeResult = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult = entityTypeResult.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult = orgCodeResult.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse destination = orgIdResult.rAKCDetails(rAKCDetails).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setDestination(destination);
        transportInstructionLegsResponse.setDropMode("ItemType");
        transportInstructionLegsResponse.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setGuid(UUID.randomUUID());
        transportInstructionLegsResponse.setId(1L);
        transportInstructionLegsResponse.setLegType("ItemType");
        PartiesResponse.PartiesResponseBuilder entityTypeResult2 = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult2 = entityTypeResult2.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult2 = orgCodeResult2.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails2 = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse origin = orgIdResult2.rAKCDetails(rAKCDetails2).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setOrigin(origin);
        transportInstructionLegsResponse.setRemarks("ItemType");
        transportInstructionLegsResponse.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setSequence(5L);

        ArrayList<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        responseList.add(transportInstructionLegsResponse);
        transportInstructionValidationUtil.addAllTiLegsMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiLegsMasterDataInSingleCallList6() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        TransportInstructionLegsResponse transportInstructionLegsResponse = new TransportInstructionLegsResponse();
        transportInstructionLegsResponse.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        PartiesResponse.PartiesResponseBuilder entityTypeResult = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult = entityTypeResult.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult = orgCodeResult.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse destination = orgIdResult.rAKCDetails(rAKCDetails).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setDestination(destination);
        transportInstructionLegsResponse.setDropMode("ItemType");
        transportInstructionLegsResponse.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setGuid(UUID.randomUUID());
        transportInstructionLegsResponse.setId(1L);
        transportInstructionLegsResponse.setLegType("ItemType");
        PartiesResponse.PartiesResponseBuilder entityTypeResult2 = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult2 = entityTypeResult2.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult2 = orgCodeResult2.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails2 = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse origin = orgIdResult2.rAKCDetails(rAKCDetails2).tenantId(1).type("Type").build();
        transportInstructionLegsResponse.setOrigin(origin);
        transportInstructionLegsResponse.setRemarks("ItemType");
        transportInstructionLegsResponse.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse.setSequence(5L);

        TransportInstructionLegsResponse transportInstructionLegsResponse2 = new TransportInstructionLegsResponse();
        transportInstructionLegsResponse2.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse2.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        PartiesResponse.PartiesResponseBuilder entityTypeResult3 = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult3 = entityTypeResult3.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult3 = orgCodeResult3.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails3 = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse destination2 = orgIdResult3.rAKCDetails(rAKCDetails3).tenantId(1).type("Type").build();
        transportInstructionLegsResponse2.setDestination(destination2);
        transportInstructionLegsResponse2.setDropMode("ItemValue");
        transportInstructionLegsResponse2.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse2.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse2.setGuid(UUID.randomUUID());
        transportInstructionLegsResponse2.setId(2L);
        transportInstructionLegsResponse2.setLegType("ItemValue");
        PartiesResponse.PartiesResponseBuilder entityTypeResult4 = PartiesResponse.builder()
                .countryCode("GB")
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult4 = entityTypeResult4.guid(UUID.randomUUID())
                .id(1L)
                .message("Not all who wander are lost")
                .orgCode("Org Code");
        PartiesResponse.PartiesResponseBuilder orgIdResult4 = orgCodeResult4.orgData(new HashMap<>()).orgId("42");
        RAKCDetailsResponse rAKCDetails4 = RAKCDetailsResponse.builder()
                .id(1L)
                .kCRAExpiry("K CRAExpiry")
                .kCRANumber("42")
                .knownConsignor(true)
                .orgId(1L)
                .orgOrganizationCode("Org Organization Code")
                .regulatedAgent(true)
                .build();
        PartiesResponse origin2 = orgIdResult4.rAKCDetails(rAKCDetails4).tenantId(1).type("Type").build();
        transportInstructionLegsResponse2.setOrigin(origin2);
        transportInstructionLegsResponse2.setRemarks("ItemValue");
        transportInstructionLegsResponse2.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        transportInstructionLegsResponse2.setSequence(1L);

        ArrayList<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        responseList.add(transportInstructionLegsResponse2);
        responseList.add(transportInstructionLegsResponse);
        transportInstructionValidationUtil.addAllTiLegsMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiPackagesMasterDataInSingleCallList() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
        ArrayList<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiPackagesMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiPackagesMasterDataInSingleCallList2() {
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any()))
                .thenThrow(new ValidationException("ItemType"));
        ArrayList<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiPackagesMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
    }

    @Test
    void testAddAllTiPackagesMasterDataInSingleCallList3() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder descriptionResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = descriptionResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder hazardLabelResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .hazardLabel("Hazard Label");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = hazardLabelResult
                .height(new BigDecimal("2.3"))
                .heightUnit("Height Unit")
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder lengthUnitResult = idResult
                .length(new BigDecimal("2.3"))
                .lengthUnit("Length Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = lengthUnitResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder volumeUnitResult = unNumberResult
                .volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit");
        TransportInstructionLegsPackagesResponse buildResult = volumeUnitResult.width(new BigDecimal("2.3"))
                .widthUnit("Width Unit")
                .build();
        responseList.add(buildResult);
        transportInstructionValidationUtil.addAllTiPackagesMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiPackagesMasterDataInSingleCallList4() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder descriptionResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = descriptionResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder hazardLabelResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .hazardLabel("Hazard Label");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = hazardLabelResult
                .height(new BigDecimal("2.3"))
                .heightUnit("Height Unit")
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder lengthUnitResult = idResult
                .length(new BigDecimal("2.3"))
                .lengthUnit("Length Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = lengthUnitResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder volumeUnitResult = unNumberResult
                .volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit");
        TransportInstructionLegsPackagesResponse buildResult2 = volumeUnitResult.width(new BigDecimal("2.3"))
                .widthUnit("Width Unit")
                .build();
        responseList.add(buildResult2);
        transportInstructionValidationUtil.addAllTiPackagesMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiPackagesMasterDataInSingleCallList5() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        MasterListRequest buildResult2 = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult2);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder descriptionResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = descriptionResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder hazardLabelResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .hazardLabel("Hazard Label");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = hazardLabelResult
                .height(new BigDecimal("2.3"))
                .heightUnit("Height Unit")
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder lengthUnitResult = idResult
                .length(new BigDecimal("2.3"))
                .lengthUnit("Length Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = lengthUnitResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder volumeUnitResult = unNumberResult
                .volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit");
        TransportInstructionLegsPackagesResponse buildResult3 = volumeUnitResult.width(new BigDecimal("2.3"))
                .widthUnit("Width Unit")
                .build();
        responseList.add(buildResult3);
        transportInstructionValidationUtil.addAllTiPackagesMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiPackagesMasterDataInSingleCallList6() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder descriptionResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = descriptionResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder hazardLabelResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .hazardLabel("Hazard Label");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = hazardLabelResult
                .height(new BigDecimal("2.3"))
                .heightUnit("Height Unit")
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder lengthUnitResult = idResult
                .length(new BigDecimal("2.3"))
                .lengthUnit("Length Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = lengthUnitResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder volumeUnitResult = unNumberResult
                .volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit");
        TransportInstructionLegsPackagesResponse buildResult = volumeUnitResult.width(new BigDecimal("2.3"))
                .widthUnit("Width Unit")
                .build();
        responseList.add(buildResult);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder descriptionResult2 = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult2 = descriptionResult2
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder hazardLabelResult2 = grossWeightUnitResult2
                .guid(UUID.randomUUID())
                .hazardLabel("Hazard Label");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult2 = hazardLabelResult2
                .height(new BigDecimal("2.3"))
                .heightUnit("Height Unit")
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder lengthUnitResult2 = idResult2
                .length(new BigDecimal("2.3"))
                .lengthUnit("Length Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult2 = lengthUnitResult2
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder volumeUnitResult2 = unNumberResult2
                .volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit");
        TransportInstructionLegsPackagesResponse buildResult2 = volumeUnitResult2.width(new BigDecimal("2.3"))
                .widthUnit("Width Unit")
                .build();
        responseList.add(buildResult2);
        transportInstructionValidationUtil.addAllTiPackagesMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiContainerMasterDataInSingleCallList() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
        ArrayList<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiContainerMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiContainerMasterDataInSingleCallList2() {
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any()))
                .thenThrow(new ValidationException("ItemType"));
        ArrayList<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiContainerMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
    }

    @Test
    void testAddAllTiContainerMasterDataInSingleCallList3() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        responseList.add(buildResult);
        transportInstructionValidationUtil.addAllTiContainerMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiContainerMasterDataInSingleCallList4() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult2 = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        responseList.add(buildResult2);
        transportInstructionValidationUtil.addAllTiContainerMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiContainerMasterDataInSingleCallList5() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        MasterListRequest buildResult2 = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult2);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult3 = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        responseList.add(buildResult3);
        transportInstructionValidationUtil.addAllTiContainerMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiContainerMasterDataInSingleCallList6() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        responseList.add(buildResult);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult2 = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult2 = dgClassResult2
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult2 = grossWeightUnitResult2
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult2 = idResult2
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .packageType("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult2 = unNumberResult2.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        responseList.add(buildResult2);
        transportInstructionValidationUtil.addAllTiContainerMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiTruckDriverMasterDataInSingleCallList() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
        ArrayList<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiTruckDriverMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiTruckDriverMasterDataInSingleCallList2() {
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any()))
                .thenThrow(new ValidationException("ItemType"));
        ArrayList<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiTruckDriverMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
    }

    @Test
    void testAddAllTiTruckDriverMasterDataInSingleCallList3() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        TransportInstructionLegsTruckDriverResponse response = TransportInstructionLegsTruckDriverResponse
                .builder()
                .driverMobileNumber("driver")
                .driverName("name")
                .trailerNumberPlate("23HR2")
                .truckOrTrailerType("SRL")
                .build();

        responseList.add(response);
        transportInstructionValidationUtil.addAllTiTruckDriverMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiTruckDriverMasterDataInSingleCallList4() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        TransportInstructionLegsTruckDriverResponse response = TransportInstructionLegsTruckDriverResponse
                .builder()
                .driverMobileNumber("driver")
                .driverName("name")
                .trailerNumberPlate("23HR2")
                .truckOrTrailerType("SRL")
                .build();

        responseList.add(response);
        transportInstructionValidationUtil.addAllTiTruckDriverMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiTruckDriverMasterDataInSingleCallList5() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        MasterListRequest buildResult2 = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult2);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        TransportInstructionLegsTruckDriverResponse response = TransportInstructionLegsTruckDriverResponse
                .builder()
                .driverMobileNumber("driver")
                .driverName("name")
                .trailerNumberPlate("23HR2")
                .truckOrTrailerType("SRL")
                .build();
        responseList.add(response);
        transportInstructionValidationUtil.addAllTiTruckDriverMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiTruckDriverMasterDataInSingleCallList6() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        TransportInstructionLegsTruckDriverResponse response = TransportInstructionLegsTruckDriverResponse
                .builder()
                .driverMobileNumber("driver")
                .driverName("name")
                .trailerNumberPlate("23HR2")
                .truckOrTrailerType("SRL")
                .build();
        responseList.add(response);

        transportInstructionValidationUtil.addAllTiTruckDriverMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiReferenceMasterDataInSingleCallList() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
        ArrayList<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiReferenceNumberMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiReferenceMasterDataInSingleCallList2() {
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any()))
                .thenThrow(new ValidationException("ItemType"));
        ArrayList<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        transportInstructionValidationUtil.addAllTiReferenceNumberMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
    }

    @Test
    void testAddAllTiReferenceMasterDataInSingleCallList3() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        TransportInstructionLegsReferenceResponse response = TransportInstructionLegsReferenceResponse
                .builder()
                .type("SRN")
                .reference("srn123")
                .build();
        responseList.add(response);
        transportInstructionValidationUtil.addAllTiReferenceNumberMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiReferenceMasterDataInSingleCallList4() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        TransportInstructionLegsReferenceResponse response = TransportInstructionLegsReferenceResponse
                .builder()
                .type("SRN")
                .reference("srn123")
                .build();
        responseList.add(response);
        transportInstructionValidationUtil.addAllTiReferenceNumberMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiReferenceMasterDataInSingleCallList5() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());

        ArrayList<MasterListRequest> masterListRequestList = new ArrayList<>();
        MasterListRequest buildResult = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult);
        MasterListRequest buildResult2 = MasterListRequest.builder()
                .Cascade("Cascade")
                .ItemType("Item Type")
                .ItemValue("42")
                .build();
        masterListRequestList.add(buildResult2);
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(masterListRequestList);
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        TransportInstructionLegsReferenceResponse response = TransportInstructionLegsReferenceResponse
                .builder()
                .type("SRN")
                .reference("srn123")
                .build();
        responseList.add(response);
        transportInstructionValidationUtil.addAllTiReferenceNumberMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }

    @Test
    void testAddAllTiReferenceMasterDataInSingleCallList6() {
        doNothing().when(commonUtils)
                .createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any()))
                .thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils)
                .pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.<Set<String>>any(),
                        Mockito.<Object>any(), Mockito.<Map<String, Object>>any());

        ArrayList<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        TransportInstructionLegsReferenceResponse response = TransportInstructionLegsReferenceResponse
                .builder()
                .type("SRN")
                .reference("srn123")
                .build();
        responseList.add(response);
        transportInstructionValidationUtil.addAllTiReferenceNumberMasterDataInSingleCallList(responseList, new HashMap<>());
        verify(commonUtils).createMasterDataKeysList(Mockito.<Set<MasterListRequest>>any(), Mockito.<Set<String>>any());
        verify(masterDataKeyUtils).setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(),
                Mockito.<String>any(), Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                Mockito.<Map<String, Object>>any());
        verify(masterDataUtils).fetchInBulkMasterList(Mockito.<MasterListRequestV2>any());
        verify(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(),
                Mockito.<Set<String>>any(), Mockito.<Object>any(), Mockito.<Map<String, Object>>any());
    }
}
