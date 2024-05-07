package com.dpw.runner.shipment.services.service.v1.util;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.*;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitFromV1Response;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
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
        var orgMap = new HashMap<String,  Map<String, Object>>();
//        orgMap.put(getParty().getOrgCode(), getParty().getOrgData());

        var addressMap = new HashMap<String, Map<String, Object>>();
//        addressMap.put(getParty().getOrgCode() + "#" + getParty().getAddressCode(), getParty().getAddressData());
        var mockV1Response = OrgAddressResponse.builder().addresses(addressMap).organizations(orgMap).build();
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
}

