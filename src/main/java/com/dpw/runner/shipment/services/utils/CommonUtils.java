package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
import java.util.stream.Collectors;

@Component
public class CommonUtils {

    private static ObjectMapper mapper;

    private static final Logger LOG = LoggerFactory.getLogger(CommonUtils.class);
    private static final String resourcePath = String.format("%s%s", System.getProperty("user.dir"), "/src/main/resources/");

    public CommonUtils(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    public static FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

    public static ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator) {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(1);
        request.setPageSize(Integer.MAX_VALUE);


        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }

    public static ListCommonRequest constructListRequestFromEntityId(Long entityId, String entityType) {
        FilterCriteria entityIdCriteria = FilterCriteria.builder()
                .innerFilter(Arrays.asList(FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("entityId")
                                        .operator("=")
                                        .value(entityId)
                                        .build()).build(),
                        FilterCriteria.builder()
                                .logicOperator("AND")
                                .criteria(Criteria.builder()
                                        .fieldName("entityType")
                                        .operator("=")
                                        .value(entityType)
                                        .build())
                                .build()))
                .build();

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(Integer.MAX_VALUE)
                .filterCriteria(Arrays.asList(entityIdCriteria))
                .build();

        return listCommonRequest;
    }

    public static <T,P> P convertToClass(T obj, Class<P> clazz) {
        return mapper.convertValue(obj, clazz);
    }

    public static <T,P extends IRunnerResponse > List<P> convertToDtoList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }

    public static <T,P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }

    public static double roundOffToTwoDecimalPlace(double number) {
        DecimalFormat decimalFormat = new DecimalFormat("#.##");
        return Double.parseDouble(decimalFormat.format(number));
    }
}
