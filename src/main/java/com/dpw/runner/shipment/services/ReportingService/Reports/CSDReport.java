package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.CSDModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DATE_OF_PRINT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TIME_OF_PRINT;

@Component
public class CSDReport extends IReport{

    private boolean isConsolidation;
    public void setIsConsolidation(boolean isConsolidation){
        this.isConsolidation = isConsolidation;
    }

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CSDModel csdModel = (CSDModel) getDocumentModel(id);
        return populateDictionary(csdModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CSDModel csdModel = new CSDModel();
        csdModel.setUsersDto(UserContext.getUser());
        if(isConsolidation){
            csdModel.setConsolidationModel(getConsolidation(id));
        }else{
            csdModel.setShipmentModel(getShipment(id));
        }
        return csdModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CSDModel csdModel = (CSDModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();

        populateUserFields(csdModel.getUsersDto(), dictionary);
        if(isConsolidation){
            populateConsolidationFields(csdModel.getConsolidationModel(), dictionary);
            populateRaKcDataConsolidation(dictionary, csdModel.getConsolidationModel());
        }
        else {
            populateShipmentFields(csdModel.getShipmentModel(), dictionary);
            populateRaKcData(dictionary, csdModel.getShipmentModel());
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        dictionary.put(DATE_OF_PRINT, StringUtility.convertToString(ConvertToDPWDateFormat(LocalDateTime.now(), v1TenantSettingsResponse.getDPWDateFormat(), true)));
        dictionary.put(TIME_OF_PRINT, StringUtility.convertToString(ConvertToDPWDateFormatWithTime(LocalDateTime.now(), v1TenantSettingsResponse.getDPWDateFormat(), true)));

        return dictionary;
    }
}
