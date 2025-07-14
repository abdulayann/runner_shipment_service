package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.MandatoryFieldException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Service
@Slf4j
public class CustomerBookingValidations {

    @Autowired
    private CommonUtils commonUtils;

    public void onSave(CustomerBooking oldEntity, CustomerBooking newEntity) {
        if (!Objects.isNull(oldEntity) && !Objects.equals(oldEntity.getBookingNumber(), newEntity.getBookingNumber())) {
            log.error("Updating Booking number from {} to {} is not allowed.", oldEntity.getBookingNumber(), newEntity.getBookingNumber());
            throw new ValidationException(String.format("Updating Booking number from: %s to: %s is not allowed.", oldEntity.getBookingNumber(), newEntity.getBookingNumber()));
        }
        validateConsigneeConsignor(newEntity);
        var tenantSettings = Optional.ofNullable(commonUtils.getCurrentTenantSettings()).orElse(V1TenantSettingsResponse.builder().build());

        validateBookingTransportMode(oldEntity, newEntity, tenantSettings);

        Boolean countryAirCargoSecurity = tenantSettings.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity) && !CommonUtils.checkAirSecurityForBooking(newEntity)) {
            throw new ValidationException("You don't have Air Security permission to create or update AIR EXP Booking.");
        }

        // FCL
        switch (newEntity.getBookingStatus()) {
            case PENDING_FOR_KYC:
                this.validateOnPendingForKyc(newEntity);
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
            default :
                log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, newEntity.getBookingStatus());
                break;
        }
    }

    private void validateOnPendingForKyc (CustomerBooking entity) {
        if (Objects.isNull(entity.getCustomer()) || (entity.getIsCustomerFreeText() && Objects.isNull(entity.getCustomer().getOrgData()))
                || (!entity.getIsCustomerFreeText() && Objects.isNull(entity.getCustomer().getOrgCode()))
                || (Boolean.TRUE.equals(entity.getIsCustomerAddressFreeText()) && Objects.isNull(entity.getCustomer().getAddressData()))
                || (!Boolean.TRUE.equals(entity.getIsCustomerAddressFreeText()) && Objects.isNull(entity.getCustomer().getAddressCode())))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Customer detail"));

        if (Objects.isNull(entity.getTransportType()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Transport Mode"));

        if (Objects.isNull(entity.getCargoType()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Cargo Type"));

    }

    private void validateConsigneeConsignor(CustomerBooking newEntity) {
        if(!Objects.isNull(newEntity.getConsignee()) && !Objects.isNull(newEntity.getConsignor()) && newEntity.getConsignee().getOrgCode() != null
                && newEntity.getConsignor().getOrgCode() != null && newEntity.getConsignor().getOrgCode().equals(newEntity.getConsignee().getOrgCode())) {
            throw new ValidationException("Consignor & Consignee parties can't be selected as same.");
        }
    }

    private void validateBookingTransportMode(CustomerBooking oldEntity, CustomerBooking newEntity, V1TenantSettingsResponse tenantSettings) {
        // If TransportModeConfig flag is ON, this block will check for the valid transport mode
        // If oldEntity is null (Create) OR transport mode is getting updated (Update)
        if (Boolean.TRUE.equals(tenantSettings.getTransportModeConfig()) && (Objects.isNull(oldEntity) || !Objects.equals(oldEntity.getTransportType(), newEntity.getTransportType()))
                    && Boolean.FALSE.equals(commonUtils.isTransportModeValid(newEntity.getTransportType(), Constants.CUSTOMER_BOOKING, tenantSettings))) {
                throw new ValidationException(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, newEntity.getTransportType()));
        }
    }

    private void validateOnReadyForShipment(CustomerBooking entity) {
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();

        if(Boolean.TRUE.equals(v1TenantSettingsResponse.getFetchRatesMandate()) && (Objects.isNull(entity.getBookingCharges()) || entity.getBookingCharges().isEmpty()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Bill charge"));
    }

    private void validateOnPendingForCreditCheck(CustomerBooking entity) {

        if (entity.getCustomer() == null || entity.getCustomer().getOrgCode() == null || entity.getCustomer().getAddressCode() == null)
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Customer detail"));

        if (Objects.isNull(entity.getIncoTerms()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Incoterms"));

        if (Objects.isNull(entity.getDirection()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Shipment Type"));

        if (Objects.isNull(entity.getServiceMode()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Service Type"));

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

        if(Set.of(Constants.CARGO_TYPE_FCL, Constants.CARGO_TYPE_FTL).contains(entity.getCargoType()) && entity.getContainersList().isEmpty()) {
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Atleast one container"));
        }

        if(Set.of(Constants.CARGO_TYPE_LTL, Constants.CARGO_TYPE_LCL).contains(entity.getCargoType()) && entity.getPackingList().isEmpty()) {
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Atleast one package"));
        }

        if ((Objects.isNull(entity.getConsignor()) || Objects.isNull(entity.getConsignor().getOrgCode()) || Objects.isNull(entity.getConsignor().getAddressCode())) && Set.of(Constants.DIRECTION_EXP, Constants.DIRECTION_DOM, Constants.DIRECTION_CTS).contains(entity.getDirection()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Consignor detail"));

        if ((Objects.isNull(entity.getConsignee()) || Objects.isNull(entity.getConsignee().getOrgCode()) || Objects.isNull(entity.getConsignee().getAddressCode())) && Objects.equals(Constants.DIRECTION_IMP, entity.getDirection()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Consignee detail"));

    }

}
