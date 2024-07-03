package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FLIGHT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FLIGHT_NUMBER;

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

        return dictionary;
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
