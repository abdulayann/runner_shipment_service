package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

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
        response.setUnlocationData(sailingInformation.getSailingScheduleData());

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
}
