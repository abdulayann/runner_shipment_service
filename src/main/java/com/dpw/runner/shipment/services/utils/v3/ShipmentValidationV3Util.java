package com.dpw.runner.shipment.services.utils.v3;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.setIsNullOrEmpty;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DpsConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ShipmentValidationV3Util {

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IDpsEventService dpsEventService;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;


    public void validTransportModeForTrasnportModeConfig(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, boolean isImportFile, V1TenantSettingsResponse tenantSettings) {
        if (Boolean.TRUE.equals(tenantSettings.getTransportModeConfig()) && Boolean.FALSE.equals(isImportFile) && (isCreate || !Objects.equals(oldEntity.getTransportMode(), shipmentDetails.getTransportMode()))
                && Boolean.FALSE.equals(commonUtils.isTransportModeValid(shipmentDetails.getTransportMode(), Constants.SHIPMENT_DETAILS, tenantSettings))) {
            throw new ValidationException(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, shipmentDetails.getTransportMode()));
        }
    }

    public void processDGValidations(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationDetails) throws RunnerException {
        if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()))
            airDGValidations(shipmentDetails, consolidationDetails);
        if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (Objects.isNull(oldEntity) || !Boolean.TRUE.equals(oldEntity.getContainsHazardous())) && !setIsNullOrEmpty(consolidationDetails)) {
            ConsolidationDetails consolidationDetails1 = consolidationDetails.iterator().next();
            dgValidations(shipmentDetails, consolidationDetails1, 0);
        }
    }

    private void airDGValidations(ShipmentDetails shipmentDetails, Set<ConsolidationDetails> consolidationDetails) throws RunnerException {
        if(!isAirDgUser()
                && !Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (!setIsNullOrEmpty(consolidationDetails) && Boolean.TRUE.equals(consolidationDetails.iterator().next().getHazardous()))) {
            throw new RunnerException("You do not have Air DG permissions to edit this as it is a part of DG Consol");
        }
    }

    private void dgValidations(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) {
        if( ((Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()) && SHIPMENT_TYPE_LCL.equals(consolidationDetails1.getContainerCategory()))
                || checkForAirDGFlag(consolidationDetails1))
                && (Boolean.TRUE.equals(consolidationDetails1.getHazardous()) || Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()))) {
            List<ConsoleShipmentMapping> consoleShipmentMapping = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails1.getId());
            if(!listIsNullOrEmpty(consoleShipmentMapping) && consoleShipmentMapping.size() + isNewConsoleAttached > 1) {
                throwErrorMaxOneShipmentAllowedInDgConsolidation(consolidationDetails1, isNewConsoleAttached);
            }
        }
    }

    private void throwErrorMaxOneShipmentAllowedInDgConsolidation(ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) {
        if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()))
            throw new ValidationException("For Ocean DG Consolidation LCL Cargo Type, and can have only 1 shipment");
        else {
            if(isNewConsoleAttached == 1)
                throw new ValidationException(String.format(CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL, consolidationDetails1.getConsolidationNumber()));
            else
                throw new ValidationException(CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS);
        }
    }

    private boolean isAirDgUser() {
        return UserContext.isAirDgUser();
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    public void validateShippedOnBoardDate(ShipmentDetails shipmentDetails) {

        LocalDateTime shippedOnboard = Objects.nonNull(shipmentDetails.getAdditionalDetails())
                ? shipmentDetails.getAdditionalDetails().getShippedOnboard() : null;

        if (Objects.nonNull(shippedOnboard) && Objects.nonNull(shipmentDetails.getCarrierDetails())) {
            LocalDateTime actualTimeOfDeparture = shipmentDetails.getCarrierDetails().getAtd();

            if (shippedOnboard.toLocalDate().isAfter(LocalDate.now())) {
                throw new ValidationException("Shipped On Board cannot be a future date.");
            }
            if (Objects.isNull(actualTimeOfDeparture)) {
                throw new ValidationException("Shipped On Board cannot be set without Actual Time of Departure(ATD).");
            }
            if (shippedOnboard.isAfter(actualTimeOfDeparture.plusDays(1))) {
                throw new ValidationException("Shipped On Board must be before or same as ATD.");
            }
        }
    }

    public void validateCarrierDetailsDates(ShipmentDetails shipmentDetails) {

        LocalDateTime actualPickupDate = Objects.nonNull(shipmentDetails.getAdditionalDetails())
                ? shipmentDetails.getAdditionalDetails().getPickupDate() : null;

        LocalDateTime actualCargoDeliveredDate = Objects.nonNull(shipmentDetails.getAdditionalDetails())
                ? shipmentDetails.getAdditionalDetails().getCargoDeliveredDate() : null;

        LocalDateTime estimatedPickupDate = Objects.nonNull(shipmentDetails.getAdditionalDetails())
                ? shipmentDetails.getAdditionalDetails().getEstimatedPickupDate() : null;

        LocalDateTime estimatedCargoDeliveryDate = shipmentDetails.getCargoDeliveryDate();

        LocalDateTime etd = Objects.nonNull(shipmentDetails.getCarrierDetails())
                ? shipmentDetails.getCarrierDetails().getEtd() : null;

        LocalDateTime atd = Objects.nonNull(shipmentDetails.getCarrierDetails())
                ? shipmentDetails.getCarrierDetails().getAtd() : null;

        LocalDateTime eta = Objects.nonNull(shipmentDetails.getCarrierDetails())
                ? shipmentDetails.getCarrierDetails().getEta() : null;

        LocalDateTime ata = Objects.nonNull(shipmentDetails.getCarrierDetails())
                ? shipmentDetails.getCarrierDetails().getAta() : null;

        if (Objects.nonNull(estimatedPickupDate) && Objects.nonNull(etd) && estimatedPickupDate.isAfter(etd)) {
            throw new ValidationException("Est. Origin Transport Date should be less than or equal to ETD");
        }

        if (Objects.nonNull(actualPickupDate) && Objects.nonNull(atd) && actualPickupDate.isAfter(atd)) {
            throw new ValidationException("Act. Origin Transport Date should be less than or equal to ATD");
        }

        if (Objects.nonNull(estimatedCargoDeliveryDate) && Objects.nonNull(eta) && estimatedCargoDeliveryDate.isBefore(eta)) {
            throw new ValidationException("Est. Destination Transport Date should be more than or equal to ETA");
        }

        if (Objects.nonNull(actualCargoDeliveredDate) && Objects.nonNull(ata) && actualCargoDeliveredDate.isAfter(ata)) {
            throw new ValidationException("Act. Destination Transport Date should be less than or equal to ATA");
        }
    }

    public void validateShipmentCreateOrUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {

        // Validation for Controlled Value
        this.validationForControlledFields(shipmentDetails);
        // Validation for cutoffFields
        this.validationForCutOffFields(shipmentDetails);
        // Validation for ETA, ETD, ATA and ATD fields
        this.validationETAETDATAATDFields(shipmentDetails, oldEntity);
        // Validation for Same as Pol Pod flag
        this.validationForPolPodFields(shipmentDetails);
    }

    public void validationForPolPodFields(ShipmentDetails shipmentDetails){
        if(Objects.isNull(shipmentDetails.getCarrierDetails())){
            return;
        }
        if (Boolean.TRUE.equals(shipmentDetails.getCarrierDetails().getIsSameAsOriginPort())
                && !Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getOrigin())) {
            throw new ValidationException("If origin is selected as same as OriginPort then value in origin and originPort should pe same");
        }
        if (Boolean.TRUE.equals(shipmentDetails.getCarrierDetails().getIsSameAsDestinationPort())
                && !Objects.equals(shipmentDetails.getCarrierDetails().getDestinationPort(), shipmentDetails.getCarrierDetails().getDestination())) {
            throw new ValidationException("If destination is selected as same as DestinationPort then value in destination and destinationPort should pe same");
        }
    }

    private void validateDPSImplication(ShipmentDetails shipmentDetails) {
        // Check if the specific implication (CONCR) is already present for the given shipment ID.
        // If true, throw a ValidationException to prevent further processing.
        if (Objects.nonNull(shipmentDetails.getId()) && Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(shipmentDetails.getId()), DpsConstants.CONCR))) {
            throw new ValidationException(DpsConstants.DPS_ERROR_1);
        }
    }

    public void validationForControlledFields(ShipmentDetails shipmentDetails) {
        if(!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && (shipmentDetails.getControlled() != null || StringUtility.isNotEmpty(shipmentDetails.getControlledReferenceNumber()))) {
            throw new ValidationException("Controlled and Controlled Reference Number can be selected for Sea Transport Mode only");
        }
        if(shipmentDetails.getControlled() != null && StringUtility.isEmpty(shipmentDetails.getControlledReferenceNumber())) {
            throw new ValidationException("If value in Controlled is selected please enter value in Controlled Reference Number");
        }
    }

    public void validationForPartnerFields(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()) && !isStringNullOrEmpty(shipmentDetails.getCoLoadBlNumber())) {
            throw new ValidationException("Update not allowed for Co-Loader/Booking Agent AWB No. for AIR shipments");
        }
        if(!Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_STD)) return;
        String coloadBlNumber = Optional.ofNullable(oldEntity).map(ShipmentDetails::getCoLoadBlNumber).orElse(null);
        String coloadBkgNumber = Optional.ofNullable(oldEntity).map(ShipmentDetails::getCoLoadBkgNumber).orElse(null);
        String masterBill = Optional.ofNullable(oldEntity).map(ShipmentDetails::getMasterBill).orElse(null);
        if(!Objects.equals(shipmentDetails.getCoLoadBlNumber(), coloadBlNumber) || !Objects.equals(shipmentDetails.getCoLoadBkgNumber(), coloadBkgNumber)) {
            throw new ValidationException("Update not allowed for Co-Loader/Booking Agent BkgNumber, BL No/AWB No. for STD shipments");
        }
        if(TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()) && !Constants.DIRECTION_DOM.equals(shipmentDetails.getDirection()) && !Objects.equals(shipmentDetails.getMasterBill(), masterBill)) {
            throw new ValidationException("Update not allowed in Mawb Number for STD shipments");
        }
    }

    public void validationETAETDATAATDFields(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (Constants.DIRECTION_DOM.equals(shipmentDetails.getDirection())) {
            return;
        }
        CarrierDetails newCarrier = shipmentDetails.getCarrierDetails();

        if (newCarrier == null && (oldEntity == null || oldEntity.getCarrierDetails() == null)) return;

        if (oldEntity == null) {
            // Create: Ensure no values are present
            validateFieldAbsent(newCarrier.getEta(), "ETA");
            validateFieldAbsent(newCarrier.getAta(), "ATA");
            validateFieldAbsent(newCarrier.getEtd(), "ETD");
            validateFieldAbsent(newCarrier.getAtd(), "ATD");
        } else {
            CarrierDetails oldCarrier = Optional.of(oldEntity)
                    .map(ShipmentDetails::getCarrierDetails)
                    .orElse(null);

            if (oldCarrier == null) oldCarrier = new CarrierDetails();
            if (newCarrier == null) newCarrier = new CarrierDetails();

            validateFieldUnchanged(newCarrier.getEta(), oldCarrier.getEta(), "ETA");
            validateFieldUnchanged(newCarrier.getAta(), oldCarrier.getAta(), "ATA");
            validateFieldUnchanged(newCarrier.getEtd(), oldCarrier.getEtd(), "ETD");
            validateFieldUnchanged(newCarrier.getAtd(), oldCarrier.getAtd(), "ATD");
        }
    }

    private void validateFieldAbsent(Object field, String fieldName) {
        if (field != null) {
            throw new ValidationException("Update not allowed for " + fieldName);
        }
    }

    private void validateFieldUnchanged(Object newVal, Object oldVal, String fieldName) {
        if (!Objects.equals(newVal, oldVal)) {
            throw new ValidationException("Update not allowed for " + fieldName);
        }
    }


    public void validationForFmcTlcFields(ShipmentDetails shipmentDetails) {
        if (!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && StringUtility.isNotEmpty(shipmentDetails.getFmcTlcId())){
            throw new ValidationException("FmcTlcId is only applicable for Sea");
        }
        if (!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode())) return;
        if(StringUtility.isEmpty(shipmentDetails.getFmcTlcId()) && shipmentDetails.getCarrierDetails() != null) {
            boolean isFmcTlcMandatory = false;
            isFmcTlcMandatory = validateFirstTwoCharEqualToUs(shipmentDetails.getCarrierDetails().getOriginLocCode());
            if(!isFmcTlcMandatory)
                isFmcTlcMandatory = validateFirstTwoCharEqualToUs(shipmentDetails.getCarrierDetails().getDestinationLocCode());
            if (isFmcTlcMandatory){
                throw new ValidationException("FmcTlcId is mandatory if one of Origin or Destination is from US");
            }
        }
    }

    private boolean validateFirstTwoCharEqualToUs(String locCode) {
        if(StringUtility.isNotEmpty(locCode) && locCode.length() >=2){
            var firstTwoChar = locCode.substring(0, 2);
            return firstTwoChar.equalsIgnoreCase("US");
        }
        return false;
    }

    public void validationForCutOffFields(ShipmentDetails shipmentDetails) {
        if ((TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) || Constants.DIRECTION_DOM.equals(shipmentDetails.getDirection())) && shipmentDetails.getLatestArrivalTime() != null){
            throw new ValidationException("LatestArrivalTime is not applicable for Sea/Domestic Shipments");
        }

        boolean isAirOrDomestic = Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()) ||
                Constants.DIRECTION_DOM.equals(shipmentDetails.getDirection());
        if (!isAirOrDomestic) return;
        // Apply these validations for AIR or Domestic Shipments
        if (shipmentDetails.getTerminalCutoff() != null)
            throw new ValidationException("TerminalCutoff is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getVerifiedGrossMassCutoff() != null)
            throw new ValidationException("VerifiedGrossMassCutoff is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getShippingInstructionCutoff() != null)
            throw new ValidationException("ShippingInstructionCutoff is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getDgCutoff() != null)
            throw new ValidationException("DgCutoff is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getReeferCutoff() != null)
            throw new ValidationException("ReeferCutoff is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getEarliestEmptyEquipmentPickUp() != null)
            throw new ValidationException("EarliestEmptyEquipmentPickUp is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getLatestFullEquipmentDeliveredToCarrier() != null)
            throw new ValidationException("LatestFullEquipmentDeliveredToCarrier is not applicable for Air/Domestic Shipments");
        if (shipmentDetails.getEarliestDropOffFullEquipmentToCarrier() != null)
            throw new ValidationException("LatestDropOffFullEquipmentToCarrier is not applicable for Air/Domestic Shipments");
    }

    public void validateBeforeSaveForEt(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsigner() != null && shipmentDetails.getConsignee().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode().equals(shipmentDetails.getConsignee().getOrgCode()))
            throw new ValidationException(ErrorConstants.SAME_SHIPPER_CONSIGNEE);

        if(!isStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)){
            if(!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            }
            else if(!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }

        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()) {
            shipmentDetails.setConsolRef(shipmentDetails.getConsolidationList().iterator().next().getReferenceNumber());
        }

    }
}
