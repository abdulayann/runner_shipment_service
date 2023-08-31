package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.MandatoryFieldException;
import com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
public class CustomerBookingValidations {

    public void onSave(CustomerBooking oldEntity, CustomerBooking newEntity) {
        // FCL
        switch (newEntity.getBookingStatus()) {
            case PENDING_FOR_KYC -> {
                if (Objects.isNull(newEntity.getCustomer()) || (newEntity.getIsCustomerFreeText() && Objects.isNull(newEntity.getCustomer().getOrgData()))
                         || (! newEntity.getIsCustomerFreeText() && Objects.isNull(newEntity.getCustomer().getOrgCode())) )
                    throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Customer detail"));
                break;
            }
            case PENDING_FOR_CREDIT_LIMIT -> {
                this.validateOnPendingForCreditCheck(newEntity);
                break;
            }
            case READY_FOR_SHIPMENT -> {
                this.validateOnPendingForCreditCheck(newEntity);
                this.validateOnReadyForShipment(newEntity);
                break;
            }
        }
    }

    private void validateOnReadyForShipment(CustomerBooking entity) {

        if (Objects.isNull(entity.getConsignee()) || Objects.isNull(entity.getConsignee().getOrgCode()) || Objects.isNull(entity.getConsignee().getAddressCode()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Consignee detail"));

        if (Objects.isNull(entity.getConsignor()) || Objects.isNull(entity.getConsignor().getOrgCode()) || Objects.isNull(entity.getConsignor().getAddressCode()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Consignor detail"));

        if (Objects.isNull(entity.getServiceMode()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Service mode"));

        if (Objects.isNull(entity.getBookingCharges()) || entity.getBookingCharges().isEmpty())
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Bill charge"));
    }

    private void validateOnPendingForCreditCheck(CustomerBooking entity) {

        if(entity.getCustomer() == null || entity.getCustomer().getOrgCode() == null || entity.getCustomer().getAddressCode() == null)
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Customer detail"));

        if (Objects.isNull(entity.getIncoTerms()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Incoterms"));

        if (Objects.isNull(entity.getCarrierDetails()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Carrier Details"));

        if (Objects.isNull(entity.getCarrierDetails().getOrigin()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Origin"));

        if (Objects.isNull(entity.getCarrierDetails().getDestination()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Destination"));

        if (Objects.isNull(entity.getCarrierDetails().getOriginPort()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "POL"));

        if (Objects.isNull(entity.getCarrierDetails().getDestinationPort()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "POD"));

        if (Objects.isNull(entity.getTransportType()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Transport Mode"));

        if (Objects.isNull(entity.getCargoType()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Cargo Type"));



    }

}
