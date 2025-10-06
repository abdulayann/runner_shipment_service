package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingBridgeRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ContainerMisMatchWarning;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingGenerationType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@Slf4j
@Component
public class CarrierBookingUtil {

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private ICarrierBookingDao carrierBookingDao;

    public String truncate(String text, int maxLength) {
        if (text == null) {
            return null;
        }
        if (text.length() > maxLength) {
            log.warn("Truncating text from {} to {} characters", text.length(), maxLength);
            return text.substring(0, maxLength);
        }
        return text;
    }

    public List<ContainerMisMatchWarning> detectContainerMismatches(
            List<Containers> consoleContainers,
            List<CommonContainers> bookingContainers) {

        Map<String, Long> consoleMap = consoleContainers.stream()
                .collect(Collectors.toMap(
                        Containers::getContainerCode,
                        Containers::getContainerCount,
                        Long::sum
                ));

        Map<String, Long> bookingMap = bookingContainers.stream()
                .collect(Collectors.toMap(
                        CommonContainers::getContainerCode,
                        CommonContainers::getCount,
                        Long::sum
                ));

        Set<String> allCodes = new HashSet<>();
        allCodes.addAll(consoleMap.keySet());
        allCodes.addAll(bookingMap.keySet());

        return allCodes.stream()
                .map(code -> {
                    int consoleCount = consoleMap.getOrDefault(code, 0L).intValue();
                    int bookingCount = bookingMap.getOrDefault(code, 0L).intValue();
                    int difference = bookingCount - consoleCount;

                    if (difference != 0) {
                        ContainerMisMatchWarning warning = new ContainerMisMatchWarning();
                        warning.setContainerCode(code);
                        warning.setBkgCount(bookingCount);
                        warning.setConsoleCount(consoleCount);
                        warning.setDifference(difference);
                        return warning;
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .toList();
    }

    public void generateBookingNumber(CarrierBooking carrierBookingEntity) {
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var prefix = "";
        if (StringUtility.isNotEmpty(v1TenantSettingsResponse.getBookingPrefix())) {
            prefix = v1TenantSettingsResponse.getBookingPrefix();
        }
        if (v1TenantSettingsResponse.getBookingNumberGeneration() != null && v1TenantSettingsResponse.getBookingNumberGeneration() == CarrierBookingGenerationType.Serial.getValue()) {
            //get total count of carrier booking for that tenant + 1, prefix+count
            Long totalCarrierBookings = carrierBookingDao.getTotalCarrierBookings();
            carrierBookingEntity.setBookingNo(prefix + totalCarrierBookings + 1);
        } else {
            //generate the random number and add prefix
            String randomBookingNumber = StringUtility.getRandomString(10);
            carrierBookingEntity.setBookingNo(prefix + randomBookingNumber);
        }
    }

    public void mapSailingToConsolidation(SailingInformation sailingInformation,
                                                 ConsolidationDetails consolidationDetails) {
        mapIfNotNull(sailingInformation::getVerifiedGrossMassCutoff, consolidationDetails::setVerifiedGrossMassCutoff);
        mapIfNotNull(sailingInformation::getReeferCutoff, consolidationDetails::setReeferCutoff);
        mapIfNotNull(sailingInformation::getShipInstructionCutoff, consolidationDetails::setShipInstructionCutoff);
        mapIfNotNull(sailingInformation::getHazardousBookingCutoff, consolidationDetails::setHazardousBookingCutoff);
        mapIfNotNull(sailingInformation::getEmptyContainerPickupCutoff, consolidationDetails::setEarliestEmptyEquPickUp);
        mapIfNotNull(sailingInformation::getLoadedContainerGateInCutoff, consolidationDetails::setTerminalCutoff);
    }

    public void mapConsolidationToSailing(ConsolidationDetails consolidationDetails,
                                                 SailingInformation sailingInformation) {
        mapIfNotNull(consolidationDetails::getVerifiedGrossMassCutoff, sailingInformation::setVerifiedGrossMassCutoff);
        mapIfNotNull(consolidationDetails::getReeferCutoff, sailingInformation::setReeferCutoff);
        mapIfNotNull(consolidationDetails::getShipInstructionCutoff, sailingInformation::setShipInstructionCutoff);
        mapIfNotNull(consolidationDetails::getHazardousBookingCutoff, sailingInformation::setHazardousBookingCutoff);
        mapIfNotNull(consolidationDetails::getEarliestEmptyEquPickUp, sailingInformation::setEmptyContainerPickupCutoff);
        mapIfNotNull(consolidationDetails::getTerminalCutoff, sailingInformation::setLoadedContainerGateInCutoff);
    }

    private static <T> void mapIfNotNull(Supplier<T> getter, Consumer<T> setter) {
        T value = getter.get();
        if (value != null) {
            setter.accept(value);
        }
    }

    public void populateCarrierDetails(Map<String, EntityTransferCarrier> carrierDatav1Map, CarrierBookingBridgeRequest carrierBookingBridgeRequest) {

        if (Objects.isNull(carrierDatav1Map)) return;

        // Process each carrier and fetch the required details
        for (Map.Entry<String, EntityTransferCarrier> entry : carrierDatav1Map.entrySet()) {
            EntityTransferCarrier carrier = entry.getValue();

            String carrierScacCode = carrier.ItemValue;
            String carrierDescription = carrier.ItemDescription;

            // Set the fetched details in the VerifiedGrossMassInttraResponse
            carrierBookingBridgeRequest.setCarrierScacCode(carrierScacCode);
            carrierBookingBridgeRequest.setCarrierDescription(carrierDescription);
        }
    }

    public void populateIntegrationCode(Map<String, EntityTransferContainerType> containerTypeMap, CarrierBookingBridgeRequest carrierBookingBridgeRequest) {

        if (Objects.isNull(containerTypeMap) || carrierBookingBridgeRequest.getContainersList() == null) return;

        for(CommonContainerResponse commonContainerResponse : carrierBookingBridgeRequest.getContainersList()){
            String containerCode = commonContainerResponse.getContainerCode();
            EntityTransferContainerType entityTransferContainerType = containerTypeMap.getOrDefault(containerCode, null);
            if(entityTransferContainerType != null){
                commonContainerResponse.setIntegrationCode(entityTransferContainerType.getIntegrationCode());
            }
        }
    }

    public void populateLocCode(Map<String, EntityTransferUnLocations> unlocationsMap, CarrierBooking carrierBooking) {
        try {
            EntityTransferUnLocations pol = unlocationsMap.get(carrierBooking.getSailingInformation().getPol());
            EntityTransferUnLocations pod = unlocationsMap.get(carrierBooking.getSailingInformation().getPod());
            EntityTransferUnLocations carrierReceiptPlace = unlocationsMap.get(carrierBooking.getSailingInformation().getCarrierReceiptPlace());
            EntityTransferUnLocations carrierDeliveryPlace = unlocationsMap.get(carrierBooking.getSailingInformation().getCarrierDeliveryPlace());
            EntityTransferUnLocations bookingOffice = unlocationsMap.get(carrierBooking.getBookingOffice());

            if (!Objects.isNull(carrierReceiptPlace))
                carrierBooking.getSailingInformation().setCarrierReceiptLocCode(carrierReceiptPlace.getLocCode());
            if (!Objects.isNull(carrierDeliveryPlace))
                carrierBooking.getSailingInformation().setCarrierDeliveryLocCode(carrierDeliveryPlace.getLocCode());
            if (!Objects.isNull(pol))
                carrierBooking.getSailingInformation().setOriginPortLocCode(pol.getLocCode());
            if (!Objects.isNull(pod))
                carrierBooking.getSailingInformation().setDestinationPortLocCode(pod.getLocCode());
            if (!Objects.isNull(bookingOffice))
                carrierBooking.setBookingOfficeLocCode(bookingOffice.getLocCode());

        } catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierBooking.getId(), e.getMessage());
        }
    }


    public List<String> getSendEmailBaseRequest(CarrierBooking carrierBooking) {
        StringBuilder emailsBuilder = new StringBuilder();

        if (carrierBooking.getInternalEmails() != null) {
            emailsBuilder.append(carrierBooking.getInternalEmails()).append(",");
        }

        emailsBuilder.append(carrierBooking.getCreateByUserEmail());

        if (!carrierBooking.getCreateByUserEmail().equalsIgnoreCase(carrierBooking.getSubmitByUserEmail())) {
            emailsBuilder.append(",").append(carrierBooking.getSubmitByUserEmail());
        }

        // Convert to list, trimming spaces and removing blanks
        return Arrays.stream(emailsBuilder.toString().split(","))
                .map(String::trim)
                .filter(email -> !email.isEmpty())
                .distinct() // optional: removes duplicates
                .toList();
    }
}
