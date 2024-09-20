package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IHblTermsConditionTemplateDao;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.entity.enums.TypeOfHblPrint;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.repository.interfaces.IHblTermsConditionTemplateRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class HblTermsConditionTemplateDao implements IHblTermsConditionTemplateDao {

    @Autowired
    private IHblTermsConditionTemplateRepository hblTermsConditionTemplateRepository;

    @Override
    public HblTermsConditionTemplate save(HblTermsConditionTemplate hblTermsConditionTemplate) {
        return hblTermsConditionTemplateRepository.save(hblTermsConditionTemplate);
    }

    private Optional<HblTermsConditionTemplate> findById(Long id) {
        return hblTermsConditionTemplateRepository.findById(id);
    }

    private void delete(HblTermsConditionTemplate hblTermsConditionTemplate) {
        hblTermsConditionTemplateRepository.delete(hblTermsConditionTemplate);
    }

    @Override
    public List<HblTermsConditionTemplate> saveAll(List<HblTermsConditionTemplate> hblTermsConditionTemplateList) {
        return hblTermsConditionTemplateRepository.saveAll(hblTermsConditionTemplateList);
    }

    private Page<HblTermsConditionTemplate> findAll(Specification<HblTermsConditionTemplate> spec, Pageable pageable) {
        return hblTermsConditionTemplateRepository.findAll(spec, pageable);
    }

    @Override
    public List<HblTermsConditionTemplate> saveEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint) {
        List<HblTermsConditionTemplate> res = new ArrayList<>();
        for (HblTermsConditionTemplate req : hblTermsConditionTemplateList) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<HblTermsConditionTemplate> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Hbl Terms Condition Template is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentSettingsId(shipmentSettingsId);
            req.setIsFrontPrint(isFrontPrint);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    @Override
    public List<HblTermsConditionTemplate> updateEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint) throws RunnerException {
        String responseMsg;
        List<HblTermsConditionTemplate> responseHblTermsConditionTemplate = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsId, "=");
            Pair<Specification<HblTermsConditionTemplate>, Pageable> pair = fetchData(listCommonRequest, HblTermsConditionTemplate.class);
            Page<HblTermsConditionTemplate> hblTermsConditionTemplates = findAll(pair.getLeft(), pair.getRight());
            List<HblTermsConditionTemplate> hashMap = hblTermsConditionTemplates.stream()
                    .filter(e -> e.getIsFrontPrint() == isFrontPrint)
                    .toList();
            List<HblTermsConditionTemplate> hblTermsConditionTemplatesRequestList = new ArrayList<>();
            if (hblTermsConditionTemplateList != null && hblTermsConditionTemplateList.size() != 0) {
                for (HblTermsConditionTemplate request : hblTermsConditionTemplateList) {
                    Long id = request.getId();
                    hblTermsConditionTemplatesRequestList.add(request);
                }
                responseHblTermsConditionTemplate = saveEntityFromSettings(hblTermsConditionTemplatesRequestList, shipmentSettingsId, isFrontPrint);
            }
            deleteHblTermsConditionTemplate(hashMap);
            return responseHblTermsConditionTemplate;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private void deleteHblTermsConditionTemplate(List<HblTermsConditionTemplate> hblTermsConditionTemplates) {
        String responseMsg;
        try {
            hblTermsConditionTemplates.forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }


    public HblTermsConditionTemplate getTemplateCode(String templateCode, Boolean pageType, String printType) {
        FilterCriteria customCriteria = FilterCriteria.builder()
                .innerFilter(Arrays.asList(FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("typeOfHblPrint")
                                        .operator("=")
                                        .value(StringUtils.isEmpty(printType) ? TypeOfHblPrint.All.name() : printType)
                                        .build()).build(),
                        FilterCriteria.builder()
                                .logicOperator("OR")
                                .criteria(Criteria.builder()
                                        .fieldName("typeOfHblPrint")
                                        .operator("=")
                                        .value(TypeOfHblPrint.All.name())
                                        .build())
                                .build()))
                .build();
        customCriteria.setLogicOperator("and");
        ListCommonRequest listCommonRequest = CommonUtils.andCriteria("templateCode", templateCode, "=", null);
        CommonUtils.andCriteria("isFrontPrint", pageType, "=", listCommonRequest);
        listCommonRequest.getFilterCriteria().get(0).getInnerFilter().add(customCriteria);

        Pair<Specification<HblTermsConditionTemplate>, Pageable> pair = fetchData(listCommonRequest, HblTermsConditionTemplate.class);
        Page<HblTermsConditionTemplate> hblTermsConditionTemplates = findAll(pair.getLeft(), pair.getRight());

        if (hblTermsConditionTemplates.getContent().size() > 0) {
            return hblTermsConditionTemplates.getContent().get(0);
        }

        return new HblTermsConditionTemplate();
    }
}
