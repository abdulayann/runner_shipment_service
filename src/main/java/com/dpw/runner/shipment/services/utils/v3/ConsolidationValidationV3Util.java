package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DpsConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;

@Slf4j
@Component
public class ConsolidationValidationV3Util {

    @Autowired
    private IDpsEventService dpsEventService;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private ConsolidationV3Util consolidationV3Util;

    /**
     * Validates that both the consolidation ID and the shipment ID list are present and valid.
     *
     * @param consolidationId the ID of the consolidation (must not be null)
     * @param shipmentIds     the list of shipment IDs to validate (must not be null or empty)
     * @throws IllegalArgumentException if either the consolidationId or shipmentIds is invalid
     */
    public void validateConsolidationIdAndShipmentIds(Long consolidationId, List<Long> shipmentIds) {
        // Check for null consolidation ID or empty shipment ID list
        if (consolidationId == null || ObjectUtils.isEmpty(shipmentIds)) {
            log.error("Validation failed: Consolidation ID or Shipment IDs are invalid. ID: {}, Shipments: {}", consolidationId, shipmentIds);
            throw new IllegalArgumentException("Consolidation ID and Shipment IDs must not be null or empty");
        }
    }

    /**
     * Performs all validations before attaching shipments to a consolidation.
     * <p>
     * This method enforces critical business rules like: - Limiting to one shipment for Ocean DG LCL consolidations - Preventing shipment attachment if already linked to a
     * different consolidation - Verifying hazardous/DG rules for air mode - Ensuring no conflicting DPS implications (like CONCR) are present on shipments
     *
     * @param consolidationDetails    The consolidation entity with current data
     * @param consoleShipmentMappings List of mappings between console and shipments
     * @param shipmentIds             List of shipment IDs being attached
     * @param consolidationId         The ID of the current consolidation
     * @param shipmentDetailsList     List of shipment entities being attached
     * @param fromConsolidation       Flag indicating if the operation is triggered from the consolidation screen
     * @throws RunnerException if any business validation fails
     */
    public void validationsBeforeAttachShipments(ConsolidationDetails consolidationDetails,
            List<ConsoleShipmentMapping> consoleShipmentMappings,
            List<Long> shipmentIds,
            Long consolidationId,
            List<ShipmentDetails> shipmentDetailsList,
            boolean fromConsolidation) throws RunnerException {

        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            if(ObjectUtils.isNotEmpty(shipmentDetails.getConsolidationList())) {
                String joinedConsolidationNumbers = shipmentDetails.getConsolidationList().stream()
                        .map(ConsolidationDetails::getConsolidationNumber).collect(Collectors.joining(","));
                throw new IllegalArgumentException("Shipment is already attached to Consolidation - "+ joinedConsolidationNumbers);
            }

            if(Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipmentDetails.getJobType()) && TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode())) {
                throw new IllegalArgumentException("Selected shipment is a Direct Shipment, Please select different shipment.");
            }

            if(Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue()) || Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.NonMovement.getValue()) ) {
                throw new IllegalArgumentException("Selected shipment is in Cancelled/ Non Movement state Shipment, Please select different shipment.");
            }
        }

        // Count existing linked shipments (null-safe)
        int existingShipments = Optional.ofNullable(consolidationDetails.getShipmentsList())
                .map(Set::size)
                .orElse(0);

        // Enforce LCL-DG rule: For Ocean DG LCL, only one shipment is allowed
        if (Boolean.TRUE.equals(consolidationDetails.getHazardous())
                && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())
                && Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode())
                && existingShipments + shipmentIds.size() > 1) {
            throw new RunnerException("For Ocean DG Consolidation LCL Cargo Type, we can have only 1 shipment");
        }

        // Check if any of the shipments is already attached to a different consolidation
        validateMultipleConsolidationAttach(consoleShipmentMappings, consolidationId);

        // Validate Air DG hazardous conditions between console and shipments
        validateAirDgHazardousForConsoleAndShipment(consolidationDetails, shipmentIds, shipmentDetailsList, fromConsolidation, existingShipments);

        // Validate DPS implications (e.g., check if CONCR implication already exists for a shipment)
        validateDPSEvent(shipmentDetailsList);
    }

        private void validateMultipleConsolidationAttach(List<ConsoleShipmentMapping> consoleShipmentMappings, Long consolidationId) throws RunnerException {
        if (!listIsNullOrEmpty(consoleShipmentMappings)) {
            for (ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
                if (!consoleShipmentMapping.getConsolidationId().equals(consolidationId)
                    && Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone())) {
                    throw new RunnerException("Multiple consolidations are attached to the shipment, please verify.");
                }
            }
        }
    }
    private void validateDPSEvent(List<ShipmentDetails> shipmentDetailsList) throws RunnerException {
        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            if (Boolean.TRUE.equals(dpsEventService.isImplicationPresent(
                Set.of(shipmentDetails.getGuid().toString()), DpsConstants.CONCR))) {
                throw new RunnerException(DpsConstants.DPS_ERROR_2 + " : " + shipmentDetails.getShipmentId());
            }
        }
    }

    /**
     * Validates Air DG (Dangerous Goods) and hazardous shipment conditions before attaching shipments to an air consolidation.
     * <p>
     * Rules enforced: - LAT Date of the consolidation must not be after the cargo delivery date of the shipment. - For Air DG shipments in inter-branch scenarios, DG is not
     * allowed. - Delegates further validation to `validateAirDgHazardousForConsole`.
     *
     * @param consolidationDetails The consolidation entity
     * @param shipmentIds          List of shipment IDs being attached
     * @param shipmentDetailsList  List of shipment entities
     * @param fromConsolidation    Flag indicating if triggered from consolidation UI
     * @param existingShipments    Count of existing shipments already attached
     * @throws RunnerException If any business rule is violated
     */
    private void validateAirDgHazardousForConsoleAndShipment(ConsolidationDetails consolidationDetails,
            List<Long> shipmentIds,
            List<ShipmentDetails> shipmentDetailsList,
            boolean fromConsolidation,
            int existingShipments) throws RunnerException {
        boolean anyInterBranchShipment = false;

        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {

            // Rule 1: LAT Date must be before or equal to Cargo Delivery Date
            if (shipmentDetails.getCargoDeliveryDate() != null
                    && consolidationDetails.getLatDate() != null
                    && consolidationDetails.getLatDate().isAfter(shipmentDetails.getCargoDeliveryDate())) {
                throw new RunnerException("Shipment " + shipmentDetails.getShipmentId() + " Cargo Delivery Date is lesser than LAT Date.");
            }

            // Rule 2: If it's an Air DG consolidation and shipment is from another branch
            if (checkForAirDGFlag(consolidationDetails)
                    && !Objects.equals(consolidationDetails.getTenantId(), shipmentDetails.getTenantId())) {
                anyInterBranchShipment = true;

                // Inter-branch Air DG shipment is not allowed
                if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                    if (fromConsolidation) {
                        throw new RunnerException(String.format(
                                AIR_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_DG_SHIPMENT,
                                shipmentDetails.getShipmentId()
                        ));
                    } else {
                        throw new RunnerException(String.format(
                                AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION,
                                consolidationDetails.getConsolidationNumber()
                        ));
                    }
                }
            }
        }

        // Additional validations specific to the consolidation as a whole
        validateAirDgHazardousForConsole(consolidationDetails, shipmentIds, fromConsolidation, existingShipments, anyInterBranchShipment);
    }

    /**
     * Validates the rules for attaching shipments to an Air DG (Dangerous Goods) consolidation.
     * <p>
     * Rules enforced: - An Air DG consolidation can have only one shipment. - Inter-branch Air DG consolidation is not allowed.
     *
     * @param consolidationDetails   The consolidation entity
     * @param shipmentIds            List of shipment IDs being attached
     * @param fromConsolidation      Flag indicating if the action is from the consolidation UI
     * @param existingShipments      Number of shipments already attached to the consolidation
     * @param anyInterBranchShipment Flag indicating if any attached shipment is from a different tenant
     * @throws RunnerException If any rule is violated
     */
    private void validateAirDgHazardousForConsole(ConsolidationDetails consolidationDetails,
            List<Long> shipmentIds,
            boolean fromConsolidation,
            int existingShipments,
            boolean anyInterBranchShipment) throws RunnerException {

        // Check if the consolidation is Air DG
        if (checkForAirDGFlag(consolidationDetails) && Boolean.TRUE.equals(consolidationDetails.getHazardous())) {

            // Rule 1: Air DG consolidation cannot have more than one shipment
            if (existingShipments + shipmentIds.size() > 1) {
                if (fromConsolidation) {
                    throw new RunnerException(AIR_DG_CONSOLIDATION_NOT_ALLOWED_MORE_THAN_ONE_SHIPMENT);
                } else {
                    throw new RunnerException(String.format(
                            CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL,
                            consolidationDetails.getConsolidationNumber()
                    ));
                }
            }

            // Rule 2: Inter-branch Air DG consolidation is not allowed
            if (anyInterBranchShipment) {
                throw new RunnerException(String.format(
                        AIR_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_DG_CONSOLIDATION,
                        consolidationDetails.getConsolidationNumber()
                ));
            }
        }
    }

    /**
     * Checks whether the current consolidation is considered an Air DG (Dangerous Goods) shipment. This is determined by: 1. A global setting (Air DG flag) being enabled in the
     * tenant context. 2. The transport mode of the consolidation being AIR.
     *
     * @param consolidationDetails The consolidation object containing transport mode info
     * @return true if Air DG checks should be applied, false otherwise
     */
    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        // First, check if Air DG processing is globally enabled via tenant settings
        if (!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag())) {
            return false;
        }

        // Return true only if the transport mode is AIR
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    public void checkCFSValidation(ConsolidationDetails consolidationDetails, Boolean isCreate, List<ShipmentDetails> shipmentDetails) throws RunnerException {
        if(!Boolean.TRUE.equals(isCreate) && consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails)) {
            if(shipmentDetails == null)
                shipmentDetails = consolidationV3Util.getShipmentsList(consolidationDetails.getId());
            if(!listIsNullOrEmpty(shipmentDetails)) {
                for(ShipmentDetails i: shipmentDetails) {
                    if(checkIfShipmentDateGreaterThanConsole(i.getShipmentGateInDate(), consolidationDetails.getCfsCutOffDate()))
                        throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
                }
            }
        }
    }

    /**
     * Checks if the shipment date is after the consolidation date.
     *
     * @param shipmentDate      the date of the shipment
     * @param consolidationDate the date of the consolidation
     * @return true if shipment date is after consolidation date, false otherwise
     */
    public boolean checkIfShipmentDateGreaterThanConsole(LocalDateTime shipmentDate, LocalDateTime consolidationDate) {
        // If either date is null, comparison can't be made
        if (shipmentDate == null || consolidationDate == null) {
            return false;
        }

        // Return true if shipmentDate is after consolidationDate
        return shipmentDate.isAfter(consolidationDate);
    }

    public void checkInterBranchPermission(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(((Objects.isNull(oldEntity) && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) || (!Objects.isNull(oldEntity)
                && !Objects.equals(consolidationDetails.getInterBranchConsole(), oldEntity.getInterBranchConsole())))
                && (!UserContext.getUser().getPermissions().containsKey(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH)
                || Boolean.FALSE.equals(UserContext.getUser().getPermissions().get(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH)))) {
            throw new ValidationException("User don't have InterBranch Consolidation Permission to change InterBranch Flag");
        }
    }

}
