package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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
        model.setUser(UserContext.getUser());
        model.setTenantModel(getTenant());
        return model;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        Map<String, Object> dictionary = new HashMap<>();
        BookingOrderModel bookingOrderModel = (BookingOrderModel) documentModel;

        populateUserFields(bookingOrderModel.getUser(), dictionary);
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

        return dictionary;
    }
}
