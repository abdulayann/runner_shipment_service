package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.PickUpOrderReportModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class PickupOrderReport extends IReport {

    @Autowired
    private HblReport hblReport;

    private Long id;

    @Override
    public Map<String, Object> getData(Long id) {
        PickUpOrderReportModel pickUpOrderReportModel = (PickUpOrderReportModel) getDocumentModel(id);
        this.id = id;
        return populateDictionary(pickUpOrderReportModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        PickUpOrderReportModel pickUpOrderReportModel = new PickUpOrderReportModel();
        pickUpOrderReportModel.shipment = getShipment(id);
        if (pickUpOrderReportModel.shipment != null && pickUpOrderReportModel.shipment.getPickupDetails() != null)
            pickUpOrderReportModel.pickUpTransportAddress = pickUpOrderReportModel.shipment.getPickupDetails().getTransporterDetail();
        return pickUpOrderReportModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        Map<String, Object> dictionary = hblReport.getData(this.id);
        PickUpOrderReportModel pickUpOrderReportModel = (PickUpOrderReportModel) documentModel;
        dictionary.put(ReportConstants.PICKUP_TRANSPORT_CONTACT_PERSON, (pickUpOrderReportModel.pickUpTransportAddress == null || pickUpOrderReportModel.pickUpTransportAddress.getAddressData() == null) ? "" :pickUpOrderReportModel.pickUpTransportAddress.getAddressData().get("ContactPerson"));
        try {
            if (pickUpOrderReportModel.shipment != null && pickUpOrderReportModel.shipment.getPickupDetails() != null) {
                PickupDeliveryDetailsModel pickupDetails = pickUpOrderReportModel.shipment.getPickupDetails();
                List<String> pickUpFrom = ReportHelper.getOrgAddress(pickupDetails.getSourceDetail());
                dictionary.put(ReportConstants.PickupFrom, pickUpFrom);

                // P0 tags pickup order doc
                if(pickupDetails.getTransporterDetail() != null) {
                    dictionary.put(ReportConstants.PICKUP_TRANSPORT_COMPANY, getValueFromMap(pickupDetails.getTransporterDetail().getOrgData(), ReportConstants.FULL_NAME));
                    dictionary.put(ReportConstants.PICKUP_TRANSPORT_CONTACT_PERSON, getValueFromMap(pickupDetails.getTransporterDetail().getAddressData(), CONTACT_PERSON));
                }
                if(pickupDetails.getSourceDetail() != null) {
                    dictionary.put(ReportConstants.PICKUP_COMPANY, getValueFromMap(pickupDetails.getSourceDetail().getOrgData(), ReportConstants.FULL_NAME));
                }
            }

            dictionary.put(ReportConstants.PRINT_USER, UserContext.getUser().getUsername());
        }
        catch (Exception ignored) {}
        return dictionary;
    }
}
