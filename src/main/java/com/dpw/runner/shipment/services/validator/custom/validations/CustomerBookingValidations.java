package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.MandatoryFieldException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
public class CustomerBookingValidations {

    public void onSave(CustomerBooking oldEntity, CustomerBooking newEntity) {
        if (!Objects.isNull(oldEntity) && !Objects.equals(oldEntity.getBookingNumber(), newEntity.getBookingNumber())) {
            log.error("Updating Booking number from {} to {} is not allowed.", oldEntity.getBookingNumber(), newEntity.getBookingNumber());
            throw new ValidationException(String.format("Updating Booking number from: %s to: %s is not allowed.", oldEntity.getBookingNumber(), newEntity.getBookingNumber()));
        }
        if(!Objects.isNull(newEntity.getConsignee()) && !Objects.isNull(newEntity.getConsignor())){
            if(newEntity.getConsignee().getOrgCode() != null && newEntity.getConsignor().getOrgCode() != null && newEntity.getConsignor().getOrgCode().equals(newEntity.getConsignee().getOrgCode()))
                throw new ValidationException("Consignor & Consignee parties can't be selected as same.");
        }
        // FCL
        switch (newEntity.getBookingStatus()) {
            case PENDING_FOR_KYC:
                if (Objects.isNull(newEntity.getCustomer()) || (newEntity.getIsCustomerFreeText() && Objects.isNull(newEntity.getCustomer().getOrgData()))
                        || (!newEntity.getIsCustomerFreeText() && Objects.isNull(newEntity.getCustomer().getOrgCode()))
                        || (newEntity.getIsCustomerAddressFreeText() && Objects.isNull(newEntity.getCustomer().getAddressData()))
                        || (!newEntity.getIsCustomerAddressFreeText() && Objects.isNull(newEntity.getCustomer().getAddressCode())))
                    throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Customer detail"));
                break;

            case PENDING_FOR_CREDIT_LIMIT:
                this.validateOnPendingForCreditCheck(newEntity);
                break;

            case READY_FOR_SHIPMENT:
                this.validateOnPendingForCreditCheck(newEntity);
                this.validateOnReadyForShipment(newEntity);
                break;
            case CANCELLED:
                break;
        }
    }

    private void validateOnReadyForShipment(CustomerBooking entity) {

        if ((Objects.isNull(entity.getConsignee()) || Objects.isNull(entity.getConsignee().getOrgCode()) || Objects.isNull(entity.getConsignee().getAddressCode())) && !Objects.equals(entity.getDirection(),Constants.DIRECTION_EXP))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Consignee detail"));

        if ((Objects.isNull(entity.getConsignor()) || Objects.isNull(entity.getConsignor().getOrgCode()) || Objects.isNull(entity.getConsignor().getAddressCode())) && !Objects.equals(entity.getDirection(), Constants.DIRECTION_IMP))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Consignor detail"));

        if (Objects.isNull(entity.getServiceMode()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Service mode"));

        if (Objects.isNull(entity.getCarrierDetails().getOriginPort()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "POL"));

        if (Objects.isNull(entity.getCarrierDetails().getDestinationPort()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "POD"));

        if (Objects.isNull(entity.getBookingCharges()) || entity.getBookingCharges().isEmpty())
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Bill charge"));
    }

    private void validateOnPendingForCreditCheck(CustomerBooking entity) {

        if (entity.getCustomer() == null || entity.getCustomer().getOrgCode() == null || entity.getCustomer().getAddressCode() == null)
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Customer detail"));

        if (Objects.isNull(entity.getIncoTerms()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Incoterms"));

        if (Objects.isNull(entity.getCarrierDetails()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Carrier Details"));

        if (Objects.isNull(entity.getCarrierDetails().getOrigin()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Origin"));

        if (Objects.isNull(entity.getCarrierDetails().getDestination()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Destination"));

        if (Objects.isNull(entity.getTransportType()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Transport Mode"));

        if (Objects.isNull(entity.getCarrierDetails().getOriginPort()) && !Objects.equals(entity.getTransportType(), Constants.TRANSPORT_MODE_AIR))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "POL"));

        if (Objects.isNull(entity.getCarrierDetails().getDestinationPort()) && !Objects.equals(entity.getTransportType(), Constants.TRANSPORT_MODE_AIR))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "POD"));

        if (Objects.isNull(entity.getCargoType()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Cargo Type"));


    }

}
