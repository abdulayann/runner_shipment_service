package com.dpw.runner.shipment.services.reportingservice.Reports;

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.*;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.reportingservice.Models.BookingOrderModel;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
public class BookingOrderReport extends IReport {
    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        IDocumentModel documentModel = getDocumentModel(id);
        return populateDictionary(documentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        BookingOrderModel model = new BookingOrderModel();
        model.setShipmentModel(getShipment(id));
        validateAirAndOceanDGCheck(model.getShipmentModel());
        model.getShipmentModel().setDocument(ReportConstants.BOOKING_ORDER);
        model.setUser(UserContext.getUser());
        model.setTenantModel(getTenant());
        return model;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        Map<String, Object> dictionary = new HashMap<>();
        BookingOrderModel bookingOrderModel = (BookingOrderModel) documentModel;

        populateUserFields(bookingOrderModel.getUser(), dictionary);
        populateTenantFields(dictionary, bookingOrderModel.getTenantModel());
        populateShipmentFields(bookingOrderModel.getShipmentModel(), dictionary);

        var transportMode = bookingOrderModel.getShipmentModel() != null ? bookingOrderModel.getShipmentModel().getTransportMode() : null;
        var direction = bookingOrderModel.getShipmentModel() != null ? bookingOrderModel.getShipmentModel().getDirection() : null;
        if((StringUtils.equals(transportMode, ReportConstants.SEA) || StringUtils.equals(transportMode, ReportConstants.AIR)) && StringUtils.equals(direction, ReportConstants.EXP)){
            String conReferenceNumberString = bookingOrderModel.getShipmentModel().getReferenceNumbersList() == null ? EMPTY_STRING : getCommaSeparatedValues(bookingOrderModel.getShipmentModel().getReferenceNumbersList());
            dictionary.put(ReportConstants.CON, conReferenceNumberString);
        }

        String shipmentType = (Objects.equals(bookingOrderModel.getShipmentModel().getJobType(), Constants.SHIPMENT_TYPE_DRT)) ? Constants.DMAWB : Constants.HAWB;
        dictionary.put(ReportConstants.SHIPMENT_TYPE, shipmentType);

        List<String> tenantNameAddress = ReportHelper.getOrgAddressWithPhoneEmail(bookingOrderModel.getTenantModel().getTenantName(),
            bookingOrderModel.getTenantModel().getAddress1(),
            bookingOrderModel.getTenantModel().getAddress2(),
            bookingOrderModel.getTenantModel().getCity(),
            null,
            bookingOrderModel.getTenantModel().getPhone(),
            bookingOrderModel.getTenantModel().getZipPostCode()
            );
        dictionary.put(ReportConstants.TENANT_NAME_AND_ADDRESS, tenantNameAddress);
        boolean isDirect = Constants.DMAWB.equals(shipmentType) ? true : false;
        boolean isNonDirect = !isDirect;
        dictionary.put(ReportConstants.IS_DIRECT_SHIPMENT, isDirect);
        dictionary.put(ReportConstants.IS_NON_DIRECT_SHIPMENT, isNonDirect);
        dictionary.put(FLIGHT_NAME, bookingOrderModel.getShipmentModel().getCarrierDetails().getShippingLine());
        dictionary.put(FLIGHT_NUMBER, bookingOrderModel.getShipmentModel().getCarrierDetails().getFlightNumber());

        // get string enclosed in parenthesis of container summary
        String containerSummary = StringUtility.convertToString(dictionary.get(ReportConstants.CONTAINER_SUMMARY));
        dictionary.put(ReportConstants.CONTAINER_SUMMARY, getStringBetweenParenthesis(containerSummary));

        if(!Objects.isNull(bookingOrderModel.getShipmentModel().getPackingList()) && !bookingOrderModel.getShipmentModel().getPackingList().isEmpty()) {
            getPackingDetails(bookingOrderModel.getShipmentModel(), dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = bookingOrderModel.getShipmentModel().getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = bookingOrderModel.getShipmentModel().getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
            if (hazardousCheck)
                dictionary.put(HAS_DANGEROUS_GOODS, true);
            else
                dictionary.put(HAS_DANGEROUS_GOODS, false);
            if (temperatureCheck)
                dictionary.put(HAS_TEMPERATURE_DETAILS, true);
            else
                dictionary.put(HAS_TEMPERATURE_DETAILS, false);

        } else {
            dictionary.put(HAS_PACK_DETAILS, false);
        }
        if(bookingOrderModel.getShipmentModel().getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(bookingOrderModel.getShipmentModel().getAdditionalDetails().getNotifyParty()));
        }

        return dictionary;
    }

    private String getCommaSeparatedValues(List<ReferenceNumbersModel> con) {
        return con.stream().filter(Objects::nonNull).filter(rf -> rf.getType().equalsIgnoreCase("CON")).map(ReferenceNumbersModel::getReferenceNumber).collect(Collectors.joining(", "));
    }

    private String getStringBetweenParenthesis(String input) {
        int length = input.length();
        boolean inParentheses = false;
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < length; i++) {
            char currentChar = input.charAt(i);
            if (currentChar == '(') {
                inParentheses = true;
            } else if (currentChar == ')' && inParentheses) {
                inParentheses = false;
            } else if (inParentheses) {
                sb.append(currentChar); // Collect characters inside parentheses
            }
        }
        return sb.toString();
    }

}
