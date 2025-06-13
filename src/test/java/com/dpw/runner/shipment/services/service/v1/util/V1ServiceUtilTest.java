package com.dpw.runner.shipment.services.service.v1.util;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitFromV1Response;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import com.dpw.runner.shipment.services.dto.v1.response.*;

@ContextConfiguration(classes = {V1ServiceUtil.class})
@ExtendWith(SpringExtension.class)
class V1ServiceUtilTest {
    private static JsonTestUtility jsonTestUtility;
    private static CustomerBooking completeCustomerBooking;
    private static Parties party;
    @MockBean
    private INotesDao iNotesDao;

    @MockBean
    private IV1Service iV1Service;

    @MockBean
    private JsonHelper jsonHelper;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @MockBean
    private CommonUtils commonUtils;
    @MockBean
    private ModelMapper modelMapper;
    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        completeCustomerBooking = jsonTestUtility.getCompleteCustomerBooking();
        party = jsonTestUtility.getCompleteShipment().getClient();
    }
    /**
     * Method under test:
     * {@link V1ServiceUtil#createBookingRequestForV1(CustomerBooking, boolean, boolean, UUID)}
     */
    @Test
    void testCreateBookingRequestForV1() {
        var note = new Notes();
        note.setText("TEXT");
        // Arrange
        when(iNotesDao.findByEntityIdAndEntityType(anyLong(), anyString())).thenReturn(List.of(note));

        // Act and Assert
        var response = v1ServiceUtil.createBookingRequestForV1(completeCustomerBooking, true, true, UUID.randomUUID());
        assertNotNull(response);
    }

    @Test
    void testCreateBookingRequestForV12() {
        var note = new Notes();
        note.setText("TEXT");
        note.setCreatedAt(LocalDateTime.now());
        // Arrange
        when(iNotesDao.findByEntityIdAndEntityType(anyLong(), anyString())).thenReturn(List.of(note));

        // Act and Assert
        var response = v1ServiceUtil.createBookingRequestForV1(completeCustomerBooking, true, true, UUID.randomUUID());
        assertNotNull(response);
    }

    @Test
    void testCreateBookingRequestForV13() {
        // Arrange
        var inputBooking = jsonTestUtility.getCompleteCustomerBooking();
        inputBooking.setContainersList(null);
        inputBooking.setRoutingList(null);
        inputBooking.setPackingList(null);
        inputBooking.setCarrierDetails(null);
        inputBooking.setNotifyParty(null);
        inputBooking.setQuantity(1);
        inputBooking.setBookingDate(null);
        inputBooking.setCustomer(null);
        inputBooking.setConsignor(null);
        inputBooking.setConsignee(null);
        inputBooking.setCargoType("FCL");
        inputBooking.setBookingCharges(null);
        when(iNotesDao.findByEntityIdAndEntityType(anyLong(), anyString())).thenReturn(null);
        // Act and Assert
        var response = v1ServiceUtil.createBookingRequestForV1(inputBooking, true, true, UUID.randomUUID());
        assertNotNull(response);
    }

    @Test
    void testCreateBookingRequestForV14() {
        // Arrange
        var inputBooking = jsonTestUtility.getCompleteCustomerBooking();
        inputBooking.setCargoType("FCL");

        when(iNotesDao.findByEntityIdAndEntityType(anyLong(), anyString())).thenReturn(null);
        // Act and Assert
        var response = v1ServiceUtil.createBookingRequestForV1(inputBooking, true, true, UUID.randomUUID());
        assertNotNull(response);
    }

    @Test
    void testCreateBookingRequestForV15() {
        var inputBooking = jsonTestUtility.getCompleteCustomerBooking();
        inputBooking.getBookingCharges().get(0).setCreditor(party);
        inputBooking.getBookingCharges().get(0).setDebtor(party);
        // Arrange
        // Act and Assert
        var response = v1ServiceUtil.createBookingRequestForV1(inputBooking, true, true, UUID.randomUUID());
        assertNotNull(response);
    }

    /**
     * Method under test:
     * {@link V1ServiceUtil#validateCreditLimit(Parties, String, UUID, Boolean)}
     */
    @Test
    void testValidateCreditLimit() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableCreditLimitManagement(true).build());
        // Arrange
        Parties client = new Parties();
        client.setAddressCode("42 Main St");
        client.setAddressData(new HashMap<>());
        client.setGuid(UUID.randomUUID());
        client.setId(1L);
        client.setOrgCode("Org Code");
        client.setOrgData(new HashMap<>());

        client.getOrgData().put("Id", 111);
        client.getAddressData().put("Id", 123);

        var mockResponse = CreditLimitValidateResponse.builder().isValid(true).taskRequiredMessage(true).message("Messasge").build();

        when(iV1Service.checkCreditLimit(any())).thenReturn(mockResponse);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());

        // Act
        CheckCreditLimitFromV1Response actualValidateCreditLimitResult = v1ServiceUtil.validateCreditLimit(client,
                "Restricted Item", UUID.randomUUID(), true);

        // Assert
        assertNull(actualValidateCreditLimitResult.getTaskRequiredMessage());
        assertNull(actualValidateCreditLimitResult.getMessage());
        assertTrue(actualValidateCreditLimitResult.getIsValid());
    }

    @Test
    void testValidateCreditLimit2() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableCreditLimitManagement(true).build());
        // Arrange
        Parties client = new Parties();
        client.setAddressCode("42 Main St");
        client.setAddressData(new HashMap<>());
        client.setGuid(UUID.randomUUID());
        client.setId(1L);
        client.setOrgCode("Org Code");
        client.setOrgData(new HashMap<>());

        var mockResponse = CreditLimitValidateResponse.builder().isValid(true).taskRequiredMessage(true).message("Messasge").build();

        when(iV1Service.checkCreditLimit(any())).thenReturn(mockResponse);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        // Act
        CheckCreditLimitFromV1Response actualValidateCreditLimitResult = v1ServiceUtil.validateCreditLimit(client,
                "Restricted Item", UUID.randomUUID(), true);

        // Assert
        assertNull(actualValidateCreditLimitResult.getTaskRequiredMessage());
        assertNull(actualValidateCreditLimitResult.getMessage());
        assertTrue(actualValidateCreditLimitResult.getIsValid());
    }

    @Test
    void testValidateCreditLimit3() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableCreditLimitManagement(true).build());
        // Arrange
        Parties client = new Parties();
        client.setAddressCode("42 Main St");
        client.setAddressData(new HashMap<>());
        client.setGuid(UUID.randomUUID());
        client.setId(1L);
        client.setOrgCode("Org Code");
        client.setOrgData(new HashMap<>());

        var mockResponse = CreditLimitValidateResponse.builder().isValid(false).taskRequiredMessage(true).message("Messasge").build();

        when(iV1Service.checkCreditLimit(any())).thenReturn(mockResponse);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());

        // Act
        CheckCreditLimitFromV1Response actualValidateCreditLimitResult = v1ServiceUtil.validateCreditLimit(client,
                "Restricted Item", UUID.randomUUID(), true);

        // Assert
        assertFalse(actualValidateCreditLimitResult.getIsValid());
    }

    @Test
    void testValidateCreditLimit4() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableCreditLimitManagement(true).build());
        // Arrange
        Parties client = new Parties();
        client.setAddressData(new HashMap<>());
        client.setOrgData(new HashMap<>());

        var mockResponse = CreditLimitValidateResponse.builder().isValid(false).taskRequiredMessage(null).message("Messasge").build();

        when(iV1Service.checkCreditLimit(any())).thenReturn(mockResponse);

        // Act
        Throwable t = assertThrows(Throwable.class, () -> v1ServiceUtil.validateCreditLimit(client, "Restricted Item", UUID.randomUUID(), true));
        // Assert
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testValidateCreditLimit5() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().build());
        // Arrange
        Parties client = new Parties();
        client.setAddressData(new HashMap<>());
        client.setOrgData(new HashMap<>());

        when(iV1Service.checkCreditLimit(any())).thenThrow(new V1ServiceException("Message"));

        // Act
        Throwable t = assertThrows(Throwable.class, () -> v1ServiceUtil.validateCreditLimit(client, "Restricted Item", UUID.randomUUID(), true));
        // Assert
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testValidateCreditLimit6() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableCreditLimitManagement(false).build());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        // Act
        CheckCreditLimitFromV1Response actualValidateCreditLimitResult = v1ServiceUtil.validateCreditLimit(Parties.builder().build(),
                "Restricted Item", UUID.randomUUID(), true);

        // Assert
        assertTrue(actualValidateCreditLimitResult.getIsValid());
    }


    /**
     * Method under test:
     * {@link V1ServiceUtil#fetchEmailIdsForShipment(ShipmentDetails)}
     */
    @Test
    void testFetchEmailIdsForShipment() {


        ShipmentDetails shipmentDetail = new ShipmentDetails();
        shipmentDetail.setConsignee(party);
        shipmentDetail.setConsigner(party);
        shipmentDetail.setClient(party);

        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        when(iV1Service.fetchOrgAddresses(any())).thenReturn( OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build());
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForShipment(shipmentDetail);

        // Assert
        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchEmailIdsForShipment2() {


        ShipmentDetails shipmentDetail = new ShipmentDetails();
        shipmentDetail.setConsignee(party);
        shipmentDetail.setConsigner(party);
        shipmentDetail.setClient(party);
        shipmentDetail.setDirection(Constants.IMP);

        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        when(iV1Service.fetchOrgAddresses(any())).thenReturn( OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build());
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForShipment(shipmentDetail);

        // Assert
        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test:
     * {@link V1ServiceUtil#fetchEmailIdsForConsolidation(ConsolidationDetails)}
//     */
    @Test
    void testFetchEmailIdsForConsolidation() {

        // Arrange
        ConsolidationDetails consolidationDetail = new ConsolidationDetails();
        consolidationDetail.setReceivingAgent(party);
        consolidationDetail.setSendingAgent(party);
        consolidationDetail.setShipmentType(Constants.DIRECTION_IMP);
        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        var mockV1Response = OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build();
        //Mock
        when(iV1Service.fetchOrgAddresses(any())).thenReturn(mockV1Response);
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetail);

        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchEmailIdsForConsolidation2() {

        // Arrange
        ConsolidationDetails consolidationDetail = new ConsolidationDetails();
        consolidationDetail.setReceivingAgent(party);
        consolidationDetail.setShipmentType(Constants.DIRECTION_EXP);
        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        var mockV1Response = OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build();
        //Mock
        when(iV1Service.fetchOrgAddresses(any())).thenReturn(mockV1Response);
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetail);

        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchEmailIdsForConsolidation3() {

        // Arrange
        var inputParty = party;
        inputParty.setOrgCode(null);
        ConsolidationDetails consolidationDetail = new ConsolidationDetails();
        consolidationDetail.setReceivingAgent(inputParty);
        consolidationDetail.setShipmentType(Constants.IMP);
        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        var mockV1Response = OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build();
        //Mock
        when(iV1Service.fetchOrgAddresses(any())).thenReturn(mockV1Response);
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetail);

        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchEmailIdsForConsolidation4() {

        // Arrange
        var inputParty = party;
        inputParty.setAddressCode(null);
        ConsolidationDetails consolidationDetail = new ConsolidationDetails();
        consolidationDetail.setReceivingAgent(inputParty);
        consolidationDetail.setShipmentType(Constants.DIRECTION_EXP);
        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        var mockV1Response = OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build();
        //Mock
        when(iV1Service.fetchOrgAddresses(any())).thenReturn(mockV1Response);
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetail);

        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchEmailIdsForConsolidation5() {

        // Arrange
        ConsolidationDetails consolidationDetail = new ConsolidationDetails();
        consolidationDetail.setReceivingAgent(party);
        consolidationDetail.setSendingAgent(party);
        consolidationDetail.setShipmentType(Constants.DIRECTION_EXP);
        var mockV1Response = OrgAddressResponse.builder().addresses(new HashMap<>()).organizations(new HashMap<>()).build();
        //Mock
        when(iV1Service.fetchOrgAddresses(any())).thenReturn(mockV1Response);
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetail);

        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchEmailIdsForConsolidation7() {

        // Arrange
        ConsolidationDetails consolidationDetail = new ConsolidationDetails();
        consolidationDetail.setReceivingAgent(party);
        consolidationDetail.setSendingAgent(party);
        consolidationDetail.setShipmentType(Constants.DIRECTION_EXP);
        var orgMap = new HashMap<String,  Map<String, Object>>();
        orgMap.put(party.getOrgCode(), party.getOrgData());
        orgMap.get(party.getOrgCode()).remove(PartiesConstants.EMAIL);
        var addressMap = new HashMap<String, Map<String, Object>>();
        addressMap.put(party.getOrgCode() + "#" + party.getAddressCode(), party.getAddressData());
        addressMap.get(party.getOrgCode() + "#" + party.getAddressCode()).remove(PartiesConstants.EMAIL);
        var mockV1Response = OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build();
        //Mock
        when(iV1Service.fetchOrgAddresses(any())).thenReturn(mockV1Response);
        // Act
        var responseEntity = v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetail);

        assertNotNull(responseEntity.getBody());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetTenantDetails1() {
        var mockResponse = new TenantDetailsByListResponse();
        var tenant1 = new TenantDetailsByListResponse.TenantDetails();
        tenant1.setTenantId(1);
        tenant1.setTenant(new HashMap<>());

        var tenant2 = new TenantDetailsByListResponse.TenantDetails();
        tenant2.setTenantId(2);
        tenant2.setTenant(new HashMap<>());
        mockResponse.setEntities(Arrays.asList(tenant1, tenant2));

        when(iV1Service.getTenantDetails(any())).thenReturn(mockResponse);

        var response = v1ServiceUtil.getTenantDetails(Arrays.asList(11));
        assertFalse(response.isEmpty());
    }

    @Test
    void testGetTenantDetails2() {
        var response = v1ServiceUtil.getTenantDetails(Arrays.asList());
        assertTrue(response.isEmpty());
    }

    @Test
    void testGetTenantDetails3() {

        when(iV1Service.getTenantDetails(any())).thenThrow(new RuntimeException());

        var response = v1ServiceUtil.getTenantDetails(Arrays.asList(11));
        assertTrue(response.isEmpty());
    }


    @Test
    void testGetTenantSettingsMap() {
        var mockResponse = new TenantDetailsByListResponse();
        var tenant1 = new TenantDetailsByListResponse.TenantDetails();
        tenant1.setTenantId(1);
        tenant1.setTenantSettings(new HashMap<>());

        var tenant2 = new TenantDetailsByListResponse.TenantDetails();
        tenant2.setTenantId(2);
        tenant2.setTenantSettings(new HashMap<>());
        mockResponse.setEntities(Arrays.asList(tenant1, tenant2));

        when(iV1Service.getTenantDetails(any())).thenReturn(mockResponse);

        var response = v1ServiceUtil.getTenantSettingsMap(Arrays.asList(11));
        assertFalse(response.isEmpty());
    }

    @Test
    void testGetTenantSettingsMap2() {
        var response = v1ServiceUtil.getTenantSettingsMap(Arrays.asList());
        assertTrue(response.isEmpty());
    }

    @Test
    void testGetTenantSettingsMap3() {

        when(iV1Service.getTenantDetails(any())).thenThrow(new RuntimeException());

        var response = v1ServiceUtil.getTenantSettingsMap(Arrays.asList(11));
        assertTrue(response.isEmpty());
    }


    @Test
    void testFetchCoLoadInfo() {
        var mockResponse = new V1DataResponse();
        mockResponse.setEntities(
                Arrays.asList(CoLoadingMAWBDetailsResponse.builder().parentTenantId(100).childTenantId(200).build(), CoLoadingMAWBDetailsResponse.builder().parentTenantId(101).childTenantId(201).build()));
        when(iV1Service.getCoLoadingStations(any())).thenReturn(mockResponse);
        when(jsonHelper.convertValueToList(any(), eq(CoLoadingMAWBDetailsResponse.class))).thenReturn(Arrays.asList(CoLoadingMAWBDetailsResponse.builder().parentTenantId(100).childTenantId(200).build(), CoLoadingMAWBDetailsResponse.builder().parentTenantId(101).childTenantId(201).build()));
        var response = v1ServiceUtil.fetchCoLoadInfo(Arrays.asList(11), "PARENT_TENANT_ID");
        assertFalse(response.isEmpty());
    }

    @Test
    void testFetchCoLoadInfo2() {
        var mockResponse = new V1DataResponse();
        when(iV1Service.getCoLoadingStations(any())).thenReturn(mockResponse);
        var response = v1ServiceUtil.fetchCoLoadInfo(Arrays.asList(), "PARENT_TENANT_ID");
        assertTrue(response.isEmpty());
    }

    @Test
    void testGetPartiesRequestFromOrgIdAndAddressId_Success() {
        Long orgId = 1L;
        Long addressId = 100L;

        V1DataResponse mockOrgResponse = new V1DataResponse();
        V1DataResponse mockAddressResponse = new V1DataResponse();
        mockOrgResponse.setEntities(new ArrayList<>());
        mockAddressResponse.setEntities(new ArrayList<>());

        List<EntityTransferOrganizations> mockOrgList = Collections.singletonList(new EntityTransferOrganizations());
        List<EntityTransferAddress> mockAddressList = Collections.singletonList(new EntityTransferAddress());

        Map<String, Object> mockOrgMap = Map.of("Id", orgId, "OrganizationCode", "Org123");
        Map<String, Object> mockAddressMap = Map.of("Id", addressId, "AddressShortCode", "Address123");

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(mockOrgList);
        when(jsonHelper.convertToJson(mockOrgList.get(0))).thenReturn("EntityOrg");
        when(jsonHelper.convertJsonToMap("EntityOrg")).thenReturn(mockOrgMap);

        when(iV1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockAddressResponse);
        when(jsonHelper.convertValueToList(mockAddressResponse.getEntities(), EntityTransferAddress.class)).thenReturn(mockAddressList);
        when(jsonHelper.convertToJson(mockAddressList.get(0))).thenReturn("EntityAddress");
        when(jsonHelper.convertJsonToMap("EntityAddress")).thenReturn(mockAddressMap);

        var result = v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(orgId, addressId);

        assertEquals(String.valueOf(orgId), result.getOrgId());
        assertEquals("Org123", result.getOrgCode());
        assertEquals(String.valueOf(addressId), result.getAddressId());
        assertEquals("Address123", result.getAddressCode());

        verify(iV1Service, times(1)).fetchOrganization(any());
        verify(iV1Service, times(1)).addressList(any());
    }

    @Test
    void testGetPartiesRequestFromOrgIdAndAddressId_NoOrganizationsFound() {
        Long orgId = 1L;
        Long addressId = 100L;

        V1DataResponse mockOrgResponse = new V1DataResponse();
        mockOrgResponse.setEntities(new ArrayList<>());

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(Collections.emptyList());

        assertThrows(DataRetrievalFailureException.class, () -> v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(orgId, addressId));

        verify(iV1Service, times(1)).fetchOrganization(any());
    }

    @Test
    void testGetPartiesRequestFromOrgIdAndAddressId_NoAddressesFound() {
        Long orgId = 1L;
        Long addressId = 100L;

        V1DataResponse mockOrgResponse = new V1DataResponse();
        List<EntityTransferOrganizations> mockOrgList = Collections.singletonList(new EntityTransferOrganizations());
        Map<String, Object> mockOrgMap = Map.of("Id", orgId, "OrganizationCode", "Org123");

        V1DataResponse mockAddressResponse = new V1DataResponse();

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(mockOrgList);
        when(jsonHelper.convertJsonToMap(anyString())).thenReturn(mockOrgMap);

        when(iV1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockAddressResponse);
        when(jsonHelper.convertValueToList(mockAddressResponse.getEntities(), EntityTransferAddress.class)).thenReturn(Collections.emptyList());

        assertThrows(DataRetrievalFailureException.class, () -> v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(orgId, addressId));

        verify(iV1Service, times(1)).fetchOrganization(any());
        verify(iV1Service, times(1)).addressList(any());
    }

    @Test
    void testGetPartiesRequestFromOrgIdAndAddressId_ExceptionThrown() {
        Long orgId = 1L;
        Long addressId = 100L;

        when(iV1Service.fetchOrganization(any())).thenThrow(new RuntimeException("Unexpected Error"));

        assertThrows(DataRetrievalFailureException.class, () -> v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(orgId, addressId));

        verify(iV1Service, times(1)).fetchOrganization(any());
    }

    @Test
    void testGetDefaultAgentOrg_WhenTenantModelIsNull() {
        Long orgId = 1L;
        Long addressId = 100L;
        V1RetrieveResponse tenantEntity = new V1RetrieveResponse();
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(orgId);
        tenantModel.setDefaultAddressId(addressId);

        V1DataResponse mockOrgResponse = new V1DataResponse();
        V1DataResponse mockAddressResponse = new V1DataResponse();
        mockOrgResponse.setEntities(new ArrayList<>());
        mockAddressResponse.setEntities(new ArrayList<>());

        List<EntityTransferOrganizations> mockOrgList = Collections.singletonList(new EntityTransferOrganizations());
        List<EntityTransferAddress> mockAddressList = Collections.singletonList(new EntityTransferAddress());

        Map<String, Object> mockOrgMap = Map.of("Id", orgId, "OrganizationCode", "Org123");
        Map<String, Object> mockAddressMap = Map.of("Id", addressId, "AddressShortCode", "Address123");

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(mockOrgList);
        when(jsonHelper.convertToJson(mockOrgList.get(0))).thenReturn("EntityOrg");
        when(jsonHelper.convertJsonToMap("EntityOrg")).thenReturn(mockOrgMap);

        when(iV1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockAddressResponse);
        when(jsonHelper.convertValueToList(mockAddressResponse.getEntities(), EntityTransferAddress.class)).thenReturn(mockAddressList);
        when(jsonHelper.convertToJson(mockAddressList.get(0))).thenReturn("EntityAddress");
        when(jsonHelper.convertJsonToMap("EntityAddress")).thenReturn(mockAddressMap);

        when(iV1Service.retrieveTenant()).thenReturn(tenantEntity);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(PartiesResponse.builder().build());

        var response = v1ServiceUtil.getDefaultAgentOrg(null);

        assertNotNull(response);

        verify(iV1Service).retrieveTenant();
        verify(modelMapper).map(any(), eq(TenantModel.class));
        verify(jsonHelper).convertValue(any(), eq(PartiesResponse.class));
    }

    @Test
    void testGetDefaultAgentOrg_WhenTenantModelHasDefaultIds() {
        Long orgId = 1L;
        Long addressId = 100L;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(orgId);
        tenantModel.setDefaultAddressId(addressId);

        V1DataResponse mockOrgResponse = new V1DataResponse();
        V1DataResponse mockAddressResponse = new V1DataResponse();
        mockOrgResponse.setEntities(new ArrayList<>());
        mockAddressResponse.setEntities(new ArrayList<>());

        List<EntityTransferOrganizations> mockOrgList = Collections.singletonList(new EntityTransferOrganizations());
        List<EntityTransferAddress> mockAddressList = Collections.singletonList(new EntityTransferAddress());

        Map<String, Object> mockOrgMap = Map.of("Id", orgId, "OrganizationCode", "Org123");
        Map<String, Object> mockAddressMap = Map.of("Id", addressId, "AddressShortCode", "Address123");

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(mockOrgList);
        when(jsonHelper.convertToJson(mockOrgList.get(0))).thenReturn("EntityOrg");
        when(jsonHelper.convertJsonToMap("EntityOrg")).thenReturn(mockOrgMap);

        when(iV1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockAddressResponse);
        when(jsonHelper.convertValueToList(mockAddressResponse.getEntities(), EntityTransferAddress.class)).thenReturn(mockAddressList);
        when(jsonHelper.convertToJson(mockAddressList.get(0))).thenReturn("EntityAddress");
        when(jsonHelper.convertJsonToMap("EntityAddress")).thenReturn(mockAddressMap);

        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(PartiesResponse.builder().build());

        var response = v1ServiceUtil.getDefaultAgentOrg(tenantModel);

        assertNotNull(response);

        verify(jsonHelper).convertValue(any(), eq(PartiesResponse.class));
        verifyNoInteractions(modelMapper);
    }

    @Test
    void testGetDefaultAgentOrg_WhenDefaultIdsAreNull() {
        TenantModel tenantModel = new TenantModel();

        var response = v1ServiceUtil.getDefaultAgentOrg(tenantModel);

        assertNull(response);

        verifyNoInteractions(jsonHelper, iV1Service, modelMapper);
    }

    @Test
    void testGetDefaultAgentOrgParty_WhenTenantModelIsNull() {
        Long orgId = 1L;
        Long addressId = 100L;
        V1RetrieveResponse tenantEntity = new V1RetrieveResponse();
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(orgId);
        tenantModel.setDefaultAddressId(addressId);

        V1DataResponse mockOrgResponse = new V1DataResponse();
        V1DataResponse mockAddressResponse = new V1DataResponse();
        mockOrgResponse.setEntities(new ArrayList<>());
        mockAddressResponse.setEntities(new ArrayList<>());

        List<EntityTransferOrganizations> mockOrgList = Collections.singletonList(new EntityTransferOrganizations());
        List<EntityTransferAddress> mockAddressList = Collections.singletonList(new EntityTransferAddress());

        Map<String, Object> mockOrgMap = Map.of("Id", orgId, "OrganizationCode", "Org123");
        Map<String, Object> mockAddressMap = Map.of("Id", addressId, "AddressShortCode", "Address123");

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(mockOrgList);
        when(jsonHelper.convertToJson(mockOrgList.get(0))).thenReturn("EntityOrg");
        when(jsonHelper.convertJsonToMap("EntityOrg")).thenReturn(mockOrgMap);

        when(iV1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockAddressResponse);
        when(jsonHelper.convertValueToList(mockAddressResponse.getEntities(), EntityTransferAddress.class)).thenReturn(mockAddressList);
        when(jsonHelper.convertToJson(mockAddressList.get(0))).thenReturn("EntityAddress");
        when(jsonHelper.convertJsonToMap("EntityAddress")).thenReturn(mockAddressMap);

        when(iV1Service.retrieveTenant()).thenReturn(tenantEntity);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(jsonHelper.convertValue(any(), eq(Parties.class))).thenReturn(Parties.builder().build());

        var response = v1ServiceUtil.getDefaultAgentOrgParty(null);

        assertNotNull(response);

        verify(iV1Service).retrieveTenant();
        verify(modelMapper).map(any(), eq(TenantModel.class));
        verify(jsonHelper).convertValue(any(), eq(Parties.class));
    }

    @Test
    void testGetDefaultAgentOrgParty_WhenTenantModelHasDefaultIds() {
        Long orgId = 1L;
        Long addressId = 100L;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(orgId);
        tenantModel.setDefaultAddressId(addressId);

        V1DataResponse mockOrgResponse = new V1DataResponse();
        V1DataResponse mockAddressResponse = new V1DataResponse();
        mockOrgResponse.setEntities(new ArrayList<>());
        mockAddressResponse.setEntities(new ArrayList<>());

        List<EntityTransferOrganizations> mockOrgList = Collections.singletonList(new EntityTransferOrganizations());
        List<EntityTransferAddress> mockAddressList = Collections.singletonList(new EntityTransferAddress());

        Map<String, Object> mockOrgMap = Map.of("Id", orgId, "OrganizationCode", "Org123");
        Map<String, Object> mockAddressMap = Map.of("Id", addressId, "AddressShortCode", "Address123");

        when(iV1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockOrgResponse);
        when(jsonHelper.convertValueToList(mockOrgResponse.getEntities(), EntityTransferOrganizations.class)).thenReturn(mockOrgList);
        when(jsonHelper.convertToJson(mockOrgList.get(0))).thenReturn("EntityOrg");
        when(jsonHelper.convertJsonToMap("EntityOrg")).thenReturn(mockOrgMap);

        when(iV1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockAddressResponse);
        when(jsonHelper.convertValueToList(mockAddressResponse.getEntities(), EntityTransferAddress.class)).thenReturn(mockAddressList);
        when(jsonHelper.convertToJson(mockAddressList.get(0))).thenReturn("EntityAddress");
        when(jsonHelper.convertJsonToMap("EntityAddress")).thenReturn(mockAddressMap);

        when(jsonHelper.convertValue(any(), eq(Parties.class))).thenReturn(Parties.builder().build());

        var response = v1ServiceUtil.getDefaultAgentOrgParty(tenantModel);

        assertNotNull(response);

        verify(jsonHelper).convertValue(any(), eq(Parties.class));
        verifyNoInteractions(modelMapper);
    }

    @Test
    void testGetDefaultAgentOrgParty_WhenDefaultIdsAreNull() {
        TenantModel tenantModel = new TenantModel();

        var response = v1ServiceUtil.getDefaultAgentOrgParty(tenantModel);

        assertNull(response);

        verifyNoInteractions(jsonHelper, iV1Service, modelMapper);
    }

    @Test
    void testGetUsersWithGivenPermission_Success() {
        List<String> permissionKeys = List.of("SHIPMENT_IN_PIPELINE_MODIFY");
        Integer tenantId = 123;
        List<UsersDto> mockUsers = List.of(new UsersDto(), new UsersDto());

        when(iV1Service.getUsersWithGivenPermissions(any())).thenReturn(mockUsers);

        List<UsersDto> result = v1ServiceUtil.getUsersWithGivenPermission(permissionKeys, tenantId);

        assertNotNull(result);
        assertEquals(2, result.size());
        verify(iV1Service, times(1)).getUsersWithGivenPermissions(any());
    }

    @Test
    void testGetUsersWithGivenPermission_EmptyResponse() {
        List<String> permissionKeys = List.of("SHIPMENT_IN_PIPELINE_MODIFY");
        Integer tenantId = 123;

        when(iV1Service.getUsersWithGivenPermissions(any())).thenReturn(List.of());

        List<UsersDto> result = v1ServiceUtil.getUsersWithGivenPermission(permissionKeys, tenantId);

        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(iV1Service, times(1)).getUsersWithGivenPermissions(any());
    }

    @Test
    void testGetUsersWithGivenPermission_NullResponse() {
        List<String> permissionKeys = List.of("SHIPMENT_IN_PIPELINE_MODIFY");
        Integer tenantId = 123;

        when(iV1Service.getUsersWithGivenPermissions(any())).thenReturn(null);

        List<UsersDto> result = v1ServiceUtil.getUsersWithGivenPermission(permissionKeys, tenantId);

        assertNull(result);
        verify(iV1Service, times(1)).getUsersWithGivenPermissions(any());
    }

    @Test
    void testGetOrganizationDataFromV1() {
        Map<String, Object> partiesMap = new HashMap<>();
        partiesMap.put(PartiesConstants.ID, 1L);
        EntityTransferOrganizations organizations = EntityTransferOrganizations.builder().build();
        when(iV1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(List.of(organizations));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(partiesMap);
        Parties parties = v1ServiceUtil.getOrganizationDataFromV1("Org");
        assertEquals("Org", parties.getOrgCode());
    }

    @Test
    void testGetOrganizationDataFromV1_exception() {
        when(iV1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(List.of());
        assertThrows(DataRetrievalFailureException.class, () -> v1ServiceUtil.getOrganizationDataFromV1("Org"));
    }

}

