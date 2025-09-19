package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassBridgeRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;
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

    public VerifiedGrossMassBridgeRequest mapToBridgeRequest(VerifiedGrossMassInttraResponse response) {
        VerifiedGrossMassBridgeRequest request = new VerifiedGrossMassBridgeRequest();

        request.setMessageGuid(response.getMessageGuid());
        request.setMessageDateTime(response.getMessageDateTime());
        request.setTenantId(response.getTenantId());
        request.setState(response.getState());
        request.setContainer(response.getContainer());
        request.setSubmitterReference(response.getSubmitterReference());
        request.setRequestor(response.getRequestor());
        request.setAuthorised(response.getAuthorised());
        request.setResponsible(response.getResponsible());
        request.setRequestorNotificationContact(response.getRequestorNotificationContact());
        request.setCarrierBookingNo(response.getCarrierBookingNo());
        request.setCarrierScacCode(response.getCarrierScacCode());
        request.setCarrierDescription(response.getCarrierDescription());
        request.setCarrierNotificationContact(response.getCarrierNotificationContact());
        request.setDelegated(response.isDelegated());
        request.setFileName(response.getFileName());

        return request;
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
