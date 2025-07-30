package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.exception.exceptions.MandatoryFieldException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Service
@Slf4j
public class CustomerBookingValidationsV3 {

    @Autowired
    private CommonUtils commonUtils;

    public void onSave(CustomerBooking oldEntity, CustomerBooking newEntity) {
        if (!Objects.isNull(oldEntity) && !Objects.equals(oldEntity.getBookingNumber(), newEntity.getBookingNumber())) {
            log.error("Updating Booking number from {} to {} is not allowed.", oldEntity.getBookingNumber(), newEntity.getBookingNumber());
            throw new ValidationException(String.format("Updating Booking number from: %s to: %s is not allowed.", oldEntity.getBookingNumber(), newEntity.getBookingNumber()));
        }
        validateDateFields(newEntity);
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

        validateMandatory(entity.getTransportType(), "Transport Mode");
        validateMandatory(entity.getCargoType(), "Cargo Type");
    }

    private void validateConsigneeConsignor(CustomerBooking newEntity) {
        if(!Objects.isNull(newEntity.getConsignee()) && !Objects.isNull(newEntity.getConsignor()) && newEntity.getConsignee().getOrgCode() != null
                && newEntity.getConsignor().getOrgCode() != null && newEntity.getConsignor().getOrgCode().equals(newEntity.getConsignee().getOrgCode())) {
            throw new ValidationException(ErrorConstants.SAME_SHIPPER_CONSIGNEE);
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
        if (Set.of(Constants.DIRECTION_EXP, Constants.DIRECTION_CTS).contains(entity.getDirection())) {
            validateParty(entity.getConsignor(), "Consignor detail");
        }
        if (Constants.DIRECTION_IMP.equals(entity.getDirection())) {
            validateParty(entity.getConsignee(), "Consignee detail");
        }
        CarrierDetails carrier = entity.getCarrierDetails();
        if(!Set.of(Constants.TRANSPORT_MODE_RAI, Constants.TRANSPORT_MODE_ROA).contains(entity.getTransportType())) {
            validateMandatory(carrier.getOriginPort(), "POL");
            validateMandatory(carrier.getDestinationPort(), "POD");
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();

        if(Boolean.TRUE.equals(v1TenantSettingsResponse.getFetchRatesMandate()) && (Objects.isNull(entity.getBookingCharges()) || entity.getBookingCharges().isEmpty()))
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "Bill charge"));
    }

    private void validateOnPendingForCreditCheck(CustomerBooking entity) {
        validateParty(entity.getCustomer(), "Customer detail");
        if (!Constants.DIRECTION_DOM.equals(entity.getDirection())) {
            validateMandatory(entity.getIncoTerms(), "Incoterms");
        }
        validateMandatory(entity.getDirection(), "Shipment Type");
        validateMandatory(entity.getServiceMode(), "Service Type");
        validateMandatory(entity.getCarrierDetails(), "Carrier Details");

        CarrierDetails carrier = entity.getCarrierDetails();
        validateMandatory(carrier.getOrigin(), "Origin");
        validateMandatory(carrier.getDestination(), "Destination");
        if (!Set.of(Constants.TRANSPORT_MODE_AIR, Constants.TRANSPORT_MODE_RAI, Constants.TRANSPORT_MODE_ROA).contains(entity.getTransportType())) {
            validateMandatory(carrier.getOriginPort(), "POL");
            validateMandatory(carrier.getDestinationPort(), "POD");
        }

        validateMandatory(entity.getTransportType(), "Transport Mode");
        validateMandatory(entity.getCargoType(), "Cargo Type");
        validateCargoContents(entity);
    }

    private void validateMandatory(Object value, String fieldName) {
        if (Objects.isNull(value)) {
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, fieldName));
        }
    }

    private void validateParty(Parties party, String fieldName) {
        if (party == null || party.getOrgCode() == null || party.getAddressCode() == null) {
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, fieldName));
        }
    }

    private void validateCargoContents(CustomerBooking entity) {
        String cargoType = entity.getCargoType();
        if (Set.of(Constants.CARGO_TYPE_FCL, Constants.CARGO_TYPE_FTL).contains(cargoType) && entity.getContainersList().isEmpty()) {
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "At least one container"));
        }
        if (Set.of(Constants.CARGO_TYPE_LTL, Constants.CARGO_TYPE_LCL).contains(cargoType) && entity.getPackingList().isEmpty()) {
            throw new MandatoryFieldException(String.format(CustomerBookingConstants.MANDATORY_FIELD, "At least one Package"));
        }
    }

    private void validateDateFields(CustomerBooking entity) {
        LocalDateTime cargoReadyDate = entity.getCargoReadinessDate();
        LocalDateTime pickupAtOriginDate = entity.getPickupAtOriginDate();
        LocalDateTime deliveryAtDestinationDate = entity.getDeliveryAtDestinationDate();
        CarrierDetails carrier = entity.getCarrierDetails();
        LocalDateTime etd = carrier != null ? carrier.getEtd() : null;
        LocalDateTime eta = carrier != null ? carrier.getEta() : null;

        validateCargoReadyDate(cargoReadyDate, etd, eta);
        validateEtdAndEta(etd, eta, cargoReadyDate);
        validatePickupAndDeliveryDates(pickupAtOriginDate, deliveryAtDestinationDate, etd, eta);
    }

    private void validateCargoReadyDate(LocalDateTime cargoReadyDate, LocalDateTime etd, LocalDateTime eta) {
        if (cargoReadyDate == null) return;

        if (etd != null && cargoReadyDate.isAfter(etd)) {
            throw new ValidationException("Cargo Ready Date must be less than or equal to ETD");
        }

        if (etd == null && eta != null && !cargoReadyDate.isBefore(eta)) {
            throw new ValidationException("Cargo Ready Date must be less than ETA");
        }
    }

    private void validateEtdAndEta(LocalDateTime etd, LocalDateTime eta, LocalDateTime cargoReadyDate) {
        if (etd == null) return;

        if (eta != null) {
            if (etd.isAfter(eta.plusHours(24))) {
                throw new ValidationException("ETD cannot be more than ETA");
            }

            if (eta.isBefore(etd.minusHours(24))) {
                throw new ValidationException("ETA cannot be less than ETD");
            }

            if (cargoReadyDate != null && (etd.isBefore(cargoReadyDate) || etd.isAfter(eta.plusHours(24)))) {
                throw new ValidationException("ETD cannot be more than ETA & less than Cargo Ready Date");
            }
        }

        if (cargoReadyDate != null && etd.isBefore(cargoReadyDate)) {
            throw new ValidationException("ETD cannot be less than Cargo Ready Date");
        }
    }

    private void validatePickupAndDeliveryDates(LocalDateTime pickup, LocalDateTime delivery, LocalDateTime etd, LocalDateTime eta) {
        if (pickup != null && etd != null && pickup.isAfter(etd)) {
            throw new ValidationException("Est. Origin Transport Date should be less than or equal to ETD");
        }

        if (delivery != null && eta != null && delivery.isBefore(eta)) {
            throw new ValidationException("Est. Destination Transport Date should be more than or equal to ETA");
        }

        if (pickup != null && delivery != null && delivery.isBefore(pickup)) {
            throw new ValidationException("Destination Transport Date must be greater than or equal to Origin Transport Date");
        }
    }
}

