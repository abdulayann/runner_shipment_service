package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.PackingListModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class PackingListReport extends IReport {

    @Autowired
    JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        PackingListModel model = (PackingListModel) getDocumentModel(id);
        return jsonHelper.convertValue(model, new TypeReference<Map<String, Object>>() {});
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        PackingListModel packingListModel = new PackingListModel();
        packingListModel.setShipmentDetails(getShipment(id));
        return packingListModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return null;
    }

    private String getLogoPath(UsersDto user)
    {
        String basePath = "Upload/";
        String path = basePath + user.TenantId + "/Assets/" + user.TenantPrintLogo;
        return path;

    }
}
