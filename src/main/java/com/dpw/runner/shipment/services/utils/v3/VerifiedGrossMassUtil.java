package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VGMContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.GROSS_WEIGHT;
import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.NET_WEIGHT;
import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.TARE_WEIGHT;

@Slf4j
@Component
public class VerifiedGrossMassUtil {

    @Autowired
    private MasterDataUtils masterDataUtils;

    public String populateRequestorEmails(VerifiedGrossMass verifiedGrossMass) {

        List<String> requestorEmailsList = new ArrayList<>();
        // Add existing external emails if any
        if (Objects.nonNull(verifiedGrossMass.getExternalEmails()) && !verifiedGrossMass.getExternalEmails().isBlank()) {
            String[] externalEmails = verifiedGrossMass.getExternalEmails().split(";");
            for (String email : externalEmails) {
                if (!email.isBlank()) {
                    requestorEmailsList.add(email.trim());
                }
            }
        }

        // Add createdBy and submitBy emails if present
        if (Objects.nonNull(verifiedGrossMass.getCreateByUserEmail()) && !verifiedGrossMass.getCreateByUserEmail().isBlank()) {
            requestorEmailsList.add(verifiedGrossMass.getCreateByUserEmail().trim());
        }

        if (Objects.nonNull(verifiedGrossMass.getSubmitByUserEmail()) && !verifiedGrossMass.getSubmitByUserEmail().isBlank()) {
            requestorEmailsList.add(verifiedGrossMass.getSubmitByUserEmail().trim());
        }

        return String.join(";", requestorEmailsList);
    }

    public CommonContainerResponse buildContainerResponse(CommonContainers container) {
        return CommonContainerResponse.builder()
                .containerNo(container.getContainerNo())
                .vgmWeight(container.getVgmWeight())
                .vgmWeightUnit(container.getVgmWeightUnit())
                .approvalSignature(container.getApprovalSignature())
                .approvalDate(container.getApprovalDate())
                .weightDeterminationMethod(container.getWeightDeterminationMethod())
                .weightDeterminationDateTime(container.getWeightDeterminationDateTime())
                // Other optional fields
                .grossWeight(container.getGrossWeight())
                .grossWeightUnit(container.getGrossWeightUnit())
                .tareWeight(container.getTareWeight())
                .tareWeightUnit(container.getTareWeightUnit())
                .sealNumber(container.getSealNumber())
                .build();
    }

    public CommonContainers buildSubmittedContainer(CommonContainers container) {
        CommonContainers submittedContainer = new CommonContainers();
        submittedContainer.setContainerRefGuid(container.getContainerRefGuid());
        submittedContainer.setContainerCode(container.getContainerCode());
        submittedContainer.setGrossWeight(container.getGrossWeight());
        submittedContainer.setNetWeight(container.getNetWeight());
        submittedContainer.setNetWeightUnit(container.getNetWeightUnit());
        submittedContainer.setGrossWeightUnit(container.getGrossWeightUnit());
        submittedContainer.setContainerNo(container.getContainerNo());
        submittedContainer.setPacks(container.getPacks());
        submittedContainer.setPacksUnit(container.getPacksUnit());
        submittedContainer.setTareWeight(container.getTareWeight());
        submittedContainer.setTareWeightUnit(container.getTareWeightUnit());
        return submittedContainer;
    }

    public Map<String,EntityTransferCarrier> fetchCarrierDetailsForBridgePayload(VerifiedGrossMass verifiedGrossMass) {

        Map<String, EntityTransferCarrier> carrierDatav1Map = new HashMap<>();
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (Objects.nonNull(verifiedGrossMass.getSailingInformation())) {
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(mapSalingInfornationToSailingInformationResponse(verifiedGrossMass.getSailingInformation()),
                        SailingInformation.class, fieldNameKeyMap, SailingInformation.class.getSimpleName(), cacheMap));
            }
            if (CollectionUtils.isEmpty(carrierList)) {
                return new HashMap<>();
            }
            carrierDatav1Map = masterDataUtils.fetchInBulkCarriers(carrierList);
            return carrierDatav1Map;

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
        return carrierDatav1Map;
    }

    public SailingInformationResponse mapSalingInfornationToSailingInformationResponse(SailingInformation sailingInformation) {
        SailingInformationResponse response = new SailingInformationResponse();
        response.setId(sailingInformation.getId());
        response.setCarrierReceiptPlace(sailingInformation.getCarrierReceiptPlace());
        response.setPol(sailingInformation.getPol());
        response.setPod(sailingInformation.getPod());
        response.setCarrierDeliveryPlace(sailingInformation.getCarrierDeliveryPlace());
        response.setCarrier(sailingInformation.getCarrier());
        response.setVesselName(sailingInformation.getVesselName());
        response.setVoyageNo(sailingInformation.getVoyageNo());
        response.setEta(sailingInformation.getEta());
        response.setEtd(sailingInformation.getEtd());
        response.setEarliestDepartureDate(sailingInformation.getEarliestDepartureDate());
        response.setLatestDeliveryDate(sailingInformation.getLatestDeliveryDate());
        response.setTerminalCutoff(sailingInformation.getTerminalCutoff());
        response.setVerifiedGrossMassCutoff(sailingInformation.getVerifiedGrossMassCutoff());
        response.setShipInstructionCutoff(sailingInformation.getShipInstructionCutoff());
        response.setHazardousBookingCutoff(sailingInformation.getHazardousBookingCutoff());
        response.setReeferCutoff(sailingInformation.getReeferCutoff());
        response.setEmptyContainerPickupCutoff(sailingInformation.getEmptyContainerPickupCutoff());
        response.setLoadedContainerGateInCutoff(sailingInformation.getLoadedContainerGateInCutoff());
        response.setLatestDeliveryDate(sailingInformation.getLatestDeliveryDate());

        return response;
    }

    public void populateCarrierDetails(Map<String, EntityTransferCarrier> carrierDatav1Map, VerifiedGrossMassInttraResponse verifiedGrossMassInttraResponse) {

        if (Objects.isNull(carrierDatav1Map)) return;

        // Process each carrier and fetch the required details
        for (Map.Entry<String, EntityTransferCarrier> entry : carrierDatav1Map.entrySet()) {
            EntityTransferCarrier carrier = entry.getValue();

            String carrierScacCode = carrier.ItemValue;
            String carrierDescription = carrier.ItemDescription;
            String carrierNotificationContact = carrier.Email;
            String carrierContactPerson = carrier.CarrierContactPerson;

            // Set the fetched details in the VerifiedGrossMassInttraResponse
            verifiedGrossMassInttraResponse.setCarrierScacCode(carrierScacCode);
            verifiedGrossMassInttraResponse.setCarrierDescription(carrierDescription);
            NotificationContactResponse notificationContactResponse = new NotificationContactResponse();
            notificationContactResponse.setUsername(carrierContactPerson);
            notificationContactResponse.setEmails(carrierNotificationContact);
            verifiedGrossMassInttraResponse.setCarrierNotificationContact(notificationContactResponse);
        }
    }


    public List<VGMContainerWarningResponse> compareVGMContainers(List<CommonContainers> currentVGMContainersList,
                                                                  List<CommonContainers> submittedVGMContainersList,
                                                                  List<Containers> consolContainersList) {

        List<VGMContainerWarningResponse> warnings = new ArrayList<>();

        try {
            if (Objects.isNull(currentVGMContainersList)) currentVGMContainersList = Collections.emptyList();
            if (Objects.isNull(submittedVGMContainersList)) submittedVGMContainersList = Collections.emptyList();
            if (Objects.isNull(consolContainersList)) consolContainersList = Collections.emptyList();

            Map<Long, CommonContainers> submittedContainersVGMMap = submittedVGMContainersList.stream()
                    .collect(Collectors.toMap(CommonContainers::getId, Function.identity(), (a, b) -> a));

            Map<Long, Containers> consolContainersMap = consolContainersList.stream()
                    .collect(Collectors.toMap(Containers::getId, Function.identity(), (a, b) -> a));

            for (CommonContainers currentContainer : currentVGMContainersList) {

                // Compare with previous submitted VGM (if exists)
                CommonContainers submittedContainer = submittedContainersVGMMap.get(currentContainer.getId());
                if (Objects.nonNull(submittedContainer) && hasAnyWeightDifference(currentContainer, submittedContainer)) {
                    warnings.add(buildVGMContainerWarning(submittedContainer, currentContainer, null));
                    
                }

                // Compare with consol container (if exists)
                Containers consolContainer = consolContainersMap.get(currentContainer.getId());
                if (Objects.nonNull(consolContainer) && hasAnyWeightDifference(currentContainer, consolContainer)) {
                    warnings.add(buildVGMContainerWarning(null, currentContainer, consolContainer));
                }
            }

        } catch (Exception exception) {
            log.error("Exception during comparing VGM container details {}", exception.getMessage(), exception);
        }

        return warnings;
    }

    private VGMContainerWarningResponse buildVGMContainerWarning(CommonContainers submittedContainer,
                                                                 CommonContainers currentContainer,
                                                                 Containers consolContainer) {
        VGMContainerWarningResponse vgmContainerWarningResponse = new VGMContainerWarningResponse();

        // Extracted logic for container number determination
        String containerNo = getContainerNumber(submittedContainer, consolContainer);
        vgmContainerWarningResponse.setContainerNumber(containerNo);

        // Extracted logic for VGM weight (Gross Weight) comparison
        String vgmOldWeight = getWeight(submittedContainer, consolContainer, GROSS_WEIGHT);
        vgmContainerWarningResponse.setVgmOldWeightValue(vgmOldWeight);
        String vgmNewWeight = formatWeight(currentContainer.getGrossWeight(), currentContainer.getGrossWeightUnit());
        vgmContainerWarningResponse.setVgmNewWeightValue(vgmNewWeight);

        // Set Gross Weight Value
        String grossOldWeight = getWeight(submittedContainer, consolContainer, GROSS_WEIGHT);
        vgmContainerWarningResponse.setOldGrossWeightValue(grossOldWeight);
        String grossNewWeight = formatWeight(currentContainer.getGrossWeight(), currentContainer.getGrossWeightUnit());
        vgmContainerWarningResponse.setNewGrossWeightValue(grossNewWeight);

        // Set Net weight Value
        String netOldWeight = getWeight(submittedContainer, consolContainer, NET_WEIGHT);
        vgmContainerWarningResponse.setOldNetWeightValue(netOldWeight);
        String netNewWeight = formatWeight(currentContainer.getNetWeight(), currentContainer.getNetWeightUnit());
        vgmContainerWarningResponse.setNewNetWeightValue(netNewWeight);

        // Tare weight comparison
        String tareOldWeight = getWeight(submittedContainer, consolContainer, TARE_WEIGHT);
        vgmContainerWarningResponse.setOldTareWeightValue(tareOldWeight);
        String tareNewWeight = formatWeight(currentContainer.getTareWeight(), currentContainer.getTareWeightUnit());
        vgmContainerWarningResponse.setNewTareWeightValue(tareNewWeight);

        return vgmContainerWarningResponse;
    }

    private String getContainerNumber(CommonContainers submittedContainer, Containers consolContainer) {
        if (Objects.nonNull(submittedContainer)) {
            return submittedContainer.getContainerNo();
        } else if (Objects.nonNull(consolContainer)) {
            return consolContainer.getContainerNumber();
        }
        return null;
    }

    private String getWeight(CommonContainers submittedContainer, Containers consolContainer, String weightType) {
        switch (weightType) {
            case GROSS_WEIGHT :
                return getFormattedWeight(submittedContainer, consolContainer);
            case NET_WEIGHT:
                return getFormattedWeight(submittedContainer, consolContainer);
            case TARE_WEIGHT:
                return getFormattedWeight(submittedContainer, consolContainer);
            default:
                return null;
        }
    }

    private String getFormattedWeight(CommonContainers submittedContainer, Containers consolContainer) {
        if (Objects.nonNull(submittedContainer)) {
            return formatWeight(submittedContainer.getGrossWeight(), submittedContainer.getGrossWeightUnit());
        } else if (Objects.nonNull(consolContainer)) {
            return formatWeight(consolContainer.getGrossWeight(), consolContainer.getGrossWeightUnit());
        }
        return null;
    }

    private boolean hasAnyWeightDifference(CommonContainers currentVGMContainer, CommonContainers submittedVGMContainer) {
        return hasWeightChanged(formatWeight(currentVGMContainer.getGrossWeight(), currentVGMContainer.getGrossWeightUnit()),
                formatWeight(submittedVGMContainer.getGrossWeight(), submittedVGMContainer.getGrossWeightUnit()))
                || hasWeightChanged(formatWeight(currentVGMContainer.getNetWeight(), currentVGMContainer.getNetWeightUnit()),
                formatWeight(submittedVGMContainer.getNetWeight(), submittedVGMContainer.getNetWeightUnit()))
                || hasWeightChanged(formatWeight(currentVGMContainer.getTareWeight(), currentVGMContainer.getTareWeightUnit()),
                formatWeight(submittedVGMContainer.getTareWeight(), submittedVGMContainer.getTareWeightUnit()));
    }

    private boolean hasAnyWeightDifference(CommonContainers currentContainer, Containers consolContainer) {
        return hasWeightChanged(formatWeight(currentContainer.getGrossWeight(), currentContainer.getGrossWeightUnit()),
                formatWeight(consolContainer.getGrossWeight(), consolContainer.getGrossWeightUnit()))
                || hasWeightChanged(formatWeight(currentContainer.getNetWeight(), currentContainer.getNetWeightUnit()),
                formatWeight(consolContainer.getNetWeight(), consolContainer.getNetWeightUnit()))
                || hasWeightChanged(formatWeight(currentContainer.getTareWeight(), currentContainer.getTareWeightUnit()),
                formatWeight(consolContainer.getTareWeight(), consolContainer.getTareWeightUnit()));
    }

    private String formatWeight(BigDecimal weight, String unit) {
        return (Objects.nonNull(weight) ? weight.toPlainString() : "") + " " + (Objects.nonNull(unit) ? unit : "");
    }

    private boolean hasWeightChanged(String currentValue, String previousValue) {
        return !StringUtils.equals(currentValue, previousValue);
    }
}
