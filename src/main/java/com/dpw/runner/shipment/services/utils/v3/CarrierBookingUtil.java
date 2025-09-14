package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ContainerMisMatchWarning;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingGenerationType;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
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
                .collect(Collectors.toList());
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
}
