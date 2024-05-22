package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.CommercialInvoiceModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Collections;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

import static com.dpw.runner.shipment.services.utils.StringUtility.getRandomString;

@Component
public class CommercialInvoiceReport extends IReport{

    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    IShipmentRepository shipmentRepository;

/**
*
   * @param id
   * @return dictionary object
*/
    @Override
    public Map<String, Object> getData(Long id) {
        // Package Count, Total amount pending
        // ShipmentProducts in v1 contains this information
        CommercialInvoiceModel model = (CommercialInvoiceModel) getDocumentModel(id);
        return jsonHelper.convertValue(model);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        CommercialInvoiceModel commercialInvoiceModel = new CommercialInvoiceModel();
        commercialInvoiceModel.setShipmentDetails(getShipment(id));
        commercialInvoiceModel.setTenant(getTenant());
        String commercialInvoiceNumber = getRandomString(7);
        commercialInvoiceModel.setCommercialInvoiceNumber(commercialInvoiceNumber);
        return commercialInvoiceModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return Collections.emptyMap();
    }
}
