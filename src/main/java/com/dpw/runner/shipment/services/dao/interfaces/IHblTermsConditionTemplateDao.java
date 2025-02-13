package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;

public interface IHblTermsConditionTemplateDao {

    HblTermsConditionTemplate save(HblTermsConditionTemplate hblTermsConditionTemplate);

    List<HblTermsConditionTemplate> saveAll(List<HblTermsConditionTemplate> hblTermsConditionTemplateList);

    List<HblTermsConditionTemplate> saveEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint);

    List<HblTermsConditionTemplate> updateEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint) throws RunnerException;

    HblTermsConditionTemplate getTemplateCode(String templateCode, Boolean pageType, String printType);

}
