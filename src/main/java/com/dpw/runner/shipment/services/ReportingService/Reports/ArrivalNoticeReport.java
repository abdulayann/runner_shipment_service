package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ArrivalNoticeModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Map;

@Component
public class ArrivalNoticeReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        ArrivalNoticeModel arrivalNoticeModel = (ArrivalNoticeModel) getDocumentModel(id);
        return populateDictionary(arrivalNoticeModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.shipmentDetails = getShipment(id);
        if(arrivalNoticeModel.shipmentDetails != null && arrivalNoticeModel.shipmentDetails.getConsolidationList() != null && !arrivalNoticeModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            arrivalNoticeModel.consolidationDetails = arrivalNoticeModel.shipmentDetails.getConsolidationList().get(0);
        }
        arrivalNoticeModel.hbl = getHbl(id);
        arrivalNoticeModel.containers = new ArrayList<>();
        if(arrivalNoticeModel.shipmentDetails.getContainersList() != null)
        {
            for(ContainerModel container : arrivalNoticeModel.shipmentDetails.getContainersList())
                arrivalNoticeModel.containers.add(getShipmentContainer(container));
        }
        arrivalNoticeModel.usersDto = UserContext.getUser();
        return arrivalNoticeModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ArrivalNoticeModel arrivalNoticeModel = (ArrivalNoticeModel) documentModel;
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(arrivalNoticeModel.shipmentDetails, GetDPWDateFormatOrDefault());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(arrivalNoticeModel.shipmentDetails, false, dictionary);
        populateUserFields(arrivalNoticeModel.usersDto, dictionary);
        populateBlFields(arrivalNoticeModel.hbl, dictionary);
        populateShipmentOrganizationsLL(arrivalNoticeModel.shipmentDetails, dictionary);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(arrivalNoticeModel.containers));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, arrivalNoticeModel.containers);
        if(StringUtility.isNotEmpty(arrivalNoticeModel.shipmentDetails.getHouseBill())) {
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_CARGOCONTROLNO, "80C2" + arrivalNoticeModel.shipmentDetails.getHouseBill());
        }
        return dictionary;
    }
}
