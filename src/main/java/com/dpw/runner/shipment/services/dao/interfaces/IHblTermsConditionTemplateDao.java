package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;

import java.util.List;

public interface IHblTermsConditionTemplateDao {

    HblTermsConditionTemplate save(HblTermsConditionTemplate hblTermsConditionTemplate);
    List<HblTermsConditionTemplate> saveAll(List<HblTermsConditionTemplate> hblTermsConditionTemplateList);
    List<HblTermsConditionTemplate> saveEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint);
    List<HblTermsConditionTemplate> updateEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint) throws Exception;
    HblTermsConditionTemplate getTemplateCode(String templateCode, int pageType, String printType);

}
