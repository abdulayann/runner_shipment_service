package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.MandatoryFieldException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import com.dpw.runner.shipment.services.CommonMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_IMP;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingValidationsV3Test extends CommonMocks {

    @InjectMocks
    CustomerBookingValidationsV3 customerBookingValidationsV3;

    @Test
    void testOnSave_differentBookingNumber_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num2").build();
        assertThrows(ValidationException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_consigneeConsignorCodeIsEqual_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code").build())
                .build();
        assertThrows(ValidationException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_PendingForKYC_nullCustomer_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_PendingForKYC_IsCustomerFreeTextIsFalse_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgData(Collections.emptyMap()).build())
                .isCustomerFreeText(false)
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_PendingForKYC_IsCustomerFreeTextIsTrue_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").orgData(Collections.emptyMap()).build())
                .isCustomerFreeText(true)
                .isCustomerAddressFreeText(true)
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_PendingForKYC_IsCustomerAddressFreeTextIsFalse_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").orgData(Collections.emptyMap()).build())
                .isCustomerFreeText(true)
                .isCustomerAddressFreeText(false)
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_PendingForKYC_without_transportType_throwsException() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").orgData(Collections.emptyMap()).build())
                .isCustomerFreeText(true)
                .isCustomerAddressFreeText(false)
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity , newEntity));
    }

    @Test
    void testOnSave_PendingForKYC_Success() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").orgData(Collections.emptyMap()).build())
                .isCustomerFreeText(true)
                .isCustomerAddressFreeText(false)
                .cargoType("FCL")
                .transportType("SEA")
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();
        assertDoesNotThrow(() -> customerBookingValidationsV3.onSave(oldEntity, newEntity));
    }

    @Test
    void testOnSave_PendingForCreditLimit() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity_withoutCustomer = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();

        CustomerBooking newEntity_withoutIncoTerms = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .direction("EXP")
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .build();

        CustomerBooking newEntity_withoutCarrierDetails = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .build();

        CustomerBooking newEntity_withoutOrigin = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .carrierDetails(CarrierDetails.builder().build())
                .build();


        CustomerBooking newEntity_withoutDestination = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .carrierDetails(CarrierDetails.builder().origin("origin").build())
                .build();

        CustomerBooking newEntity_withoutTransportType = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .carrierDetails(CarrierDetails.builder().origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutOriginPort = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .transportType("sea")
                .carrierDetails(CarrierDetails.builder().origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withOriginPortAndTransportType_ROA = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").addressCode("addressCode2").build())
                .direction("EXP")
                .incoTerms("incoterms")
                .serviceMode("P2P")
                .transportType(Constants.TRANSPORT_MODE_ROA)
                .carrierDetails(CarrierDetails.builder().originPort("origin Port").destinationPort("destination port").origin("origin").destination("destination").build())
                .cargoType("cargo_type")
                .build();

        CustomerBooking newEntity_withOriginPortAndDestinationPortAndTransportType_ROA = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").addressCode("addressCode2").build())
                .direction("EXP")
                .incoTerms("incoterms")
                .serviceMode("P2P")
                .transportType(Constants.TRANSPORT_MODE_ROA)
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .cargoType("cargo_type")
                .build();

        CustomerBooking newEntity_withoutDestinationPort = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .transportType("air")
                .carrierDetails(CarrierDetails.builder().originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutCargoType = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .transportType("air")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").addressCode("addressCode2").build())
                .direction("EXP")
                .incoTerms("incoterms")
                .serviceMode("P2P")
                .packingList(List.of(new Packing()))
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("LCL")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_With_Containers = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").addressCode("addressCode2").build())
                .direction("EXP")
                .incoTerms("incoterms")
                .serviceMode("P2P")
                .containersList(List.of(new Containers()))
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("FCL")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withOutContainers = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").addressCode("addressCode2").build())
                .direction("EXP")
                .incoTerms("incoterms")
                .serviceMode("P2P")
                .containersList(new ArrayList<>())
                .transportType(Constants.TRANSPORT_MODE_ROA)
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .cargoType("FCL")
                .build();

        CustomerBooking newEntity_withOutPackings = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").addressCode("addressCode2").build())
                .direction("EXP")
                .incoTerms("incoterms")
                .serviceMode("P2P")
                .packingList(new ArrayList<>())
                .transportType(Constants.TRANSPORT_MODE_ROA)
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .cargoType("LCL")
                .build();

        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutCustomer));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutIncoTerms));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutCarrierDetails));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutOrigin));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutDestination));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutTransportType));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutOriginPort));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutDestinationPort));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutCargoType));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutCargoType));
        assertDoesNotThrow(() -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withOriginPortAndTransportType_ROA));
        assertDoesNotThrow(() -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withOriginPortAndDestinationPortAndTransportType_ROA));
        customerBookingValidationsV3.onSave(oldEntity,newEntity);
        customerBookingValidationsV3.onSave(oldEntity,newEntity_With_Containers);
    }


    @Test
    void testOnSave_ReadyForShipment() {
        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1").build();
        CustomerBooking newEntity_withoutConsignee = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutConsignor = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(null)
                .incoTerms("incoterms")
                .direction(DIRECTION_EXP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutServiceMode = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutCarrierOriginPort = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .serviceMode("service mode")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort(null).origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutCarrierDestinationPort = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .serviceMode("service mode")
                .carrierDetails(CarrierDetails.builder().originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutBookingCharges = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .bookingCharges(null)
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .serviceMode("service mode")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withBookingCharges = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .bookingCharges(List.of(new BookingCharges()))
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .cargoType("cargo_type")
                .serviceMode("service mode")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutContainers = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .bookingCharges(List.of(new BookingCharges()))
                .cargoType("FCL")
                .containersList(new ArrayList<>())
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .serviceMode("service mode")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        CustomerBooking newEntity_withoutPackages = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.READY_FOR_SHIPMENT)
                .consignee(Parties.builder().addressCode("addressCode").orgCode("code").build())
                .customer(Parties.builder().orgCode("cide").addressCode("addressCode").build())
                .consignor(Parties.builder().orgCode("code2").build())
                .incoTerms("incoterms")
                .bookingCharges(List.of(new BookingCharges()))
                .cargoType("LCL")
                .packingList(new ArrayList<>())
                .direction(DIRECTION_IMP)
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .serviceMode("service mode")
                .carrierDetails(CarrierDetails.builder().destinationPort("destination port").originPort("origin Port").origin("origin").destination("destination").build())
                .build();

        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutConsignee));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutConsignor));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutServiceMode));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutCarrierDestinationPort));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutCarrierOriginPort));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutContainers));
        assertThrows(MandatoryFieldException.class , () -> customerBookingValidationsV3.onSave(oldEntity,newEntity_withoutPackages));
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().FetchRatesMandate(true).build());
        mockTenantSettings();
        customerBookingValidationsV3.onSave(oldEntity,newEntity_withBookingCharges);

    }

    @Test
    void testOnSaveForTransportModeConfig() {
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").build())
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .build();
        var mockTenantSettings = V1TenantSettingsResponse.builder().transportModeConfig(true).build();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(mockTenantSettings);
        when(commonUtils.isTransportModeValid(any(), anyString(), any())).thenReturn(false);

        assertThrows(ValidationException.class, () -> customerBookingValidationsV3.onSave(null, newEntity));
    }

    @Test
    void testOnSaveForTransportModeConfig2() {
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").build())
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .build();

        CustomerBooking oldEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").build())
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .build();

        var mockTenantSettings = V1TenantSettingsResponse.builder().transportModeConfig(true).build();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(mockTenantSettings);
        when(commonUtils.isTransportModeValid(any(), anyString(), any())).thenReturn(false);

        assertThrows(ValidationException.class, () -> customerBookingValidationsV3.onSave(oldEntity, newEntity));
    }

    @Test
    void testOnSaveForTransportModeConfig3() {
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.CANCELLED)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").build())
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .build();

        var mockTenantSettings = V1TenantSettingsResponse.builder().transportModeConfig(true).build();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(mockTenantSettings);
        customerBookingValidationsV3.onSave(newEntity, newEntity);
        assertNotNull(newEntity);
    }

    @Test
    void testOnSaveForTransportModeConfig4() {
        CustomerBooking newEntity = CustomerBooking.builder().bookingNumber("num1")
                .bookingStatus(BookingStatus.CANCELLED)
                .customer(Parties.builder().orgCode("code").addressCode("addressCode").build())
                .transportType(Constants.TRANSPORT_MODE_AIR)
                .build();
        var mockTenantSettings = V1TenantSettingsResponse.builder().transportModeConfig(true).build();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(mockTenantSettings);
        when(commonUtils.isTransportModeValid(any(), anyString(), any())).thenReturn(true);
        customerBookingValidationsV3.onSave(null, newEntity);
        assertNotNull(newEntity);
    }

}
