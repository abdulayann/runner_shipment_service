package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.dto.request.ShipmentMapping;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import javax.persistence.criteria.*;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.springframework.data.jpa.domain.Specification.where;

@Repository
public interface IShipmentDao extends MultiTenancyRepository<ShipmentDetails>, PermissionsRepository<ShipmentDetails> {

    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    Map<String, ShipmentMapping> tableNames = Map.ofEntries(
            Map.entry("type", ShipmentMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("orgId", ShipmentMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("houseBill", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("hblType", ShipmentMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("transportMode", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("releaseType", ShipmentMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", ShipmentMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("direction", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentType", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("status", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("source", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("jobType", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("serviceType", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("masterBill", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("bookingReference", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("consolRef", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("salesAgent", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(Long.class).build()),
            Map.entry("paymentTerms", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("incoterms", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentId", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("isDomestic", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).build()),
            Map.entry("assignedTo", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("additionalTerms", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("goodsDescription", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("createdAt", ShipmentMapping.builder().tableName("ShipmentDetails").dataType(Date.class).build()),
            Map.entry("estimatedPickup", ShipmentMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("actualPickup", ShipmentMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("estimatedDelivery", ShipmentMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("requiredBy", ShipmentMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("addressId", ShipmentMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("screeningStatus", ShipmentMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("paidPlace", ShipmentMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("placeOfIssue", ShipmentMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("dateOfIssue", ShipmentMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("dateOfReceipt", ShipmentMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("goodsCo", ShipmentMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("boeDate", ShipmentMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("boeNumber", ShipmentMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("shippingLine", ShipmentMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", ShipmentMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", ShipmentMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", ShipmentMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", ShipmentMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", ShipmentMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", ShipmentMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", ShipmentMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("weight", ShipmentMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("weightUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volume", ShipmentMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumeUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volumetricWeight", ShipmentMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumetricWeightUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("chargable", ShipmentMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("chargeableUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("netWeight", ShipmentMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("netWeightUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("noOfPacks", ShipmentMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("packsUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("innerPacks", ShipmentMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("innerPackUnit", ShipmentMapping.builder().tableName("measurementDetails").dataType(String.class).build())
    );


    static Specification<ShipmentDetails> fetchShipmentData(List<FilterCriteria> filterCriteria, SortRequest sortRequest) {
        Specification<ShipmentDetails> specification = null;
        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();
        for (FilterCriteria filters : filterCriteria) {
            if(filters.getLogicOperator() == null) {
                specification =
                        where(getSpecificationFromFilters(filters.getInnerFilter(), sortRequest, map));
            } else if (filters.getLogicOperator().equalsIgnoreCase("OR")) {
                specification = specification.or(getSpecificationFromFilters(filters.getInnerFilter(), null, map));
            } else if (filters.getLogicOperator().equalsIgnoreCase("AND")) {
                specification = specification.and(getSpecificationFromFilters(filters.getInnerFilter(), null, map));
            }
        }
        return specification;
    }


    public static <T> Specification<T> createSpecification(Criteria input, SortRequest sortRequest, Map<String, Join<Class, ShipmentDetails>> map) {
        return (root, query, criteriaBuilder) -> {
            Path path = null;

            if (tableNames.get(input.getFieldName()).getTableName().equalsIgnoreCase(ShipmentDetails.class.getSimpleName())) {
                path = root;
            } else {
                if (root.getJoins() == null || root.getJoins().size() == 0 || map.get(tableNames.get(input.getFieldName()).getTableName()) == null ||
                        !root.getJoins().contains(map.get(tableNames.get(input.getFieldName()).getTableName()))) {
                    Join<Class, ShipmentDetails> join = root.join(tableNames.get(input.getFieldName()).getTableName(), JoinType.LEFT);
                    map.put(tableNames.get(input.getFieldName()).getTableName(), join);
                    path = join;
                    query.distinct(true);
                }
                else {
                    path = map.get(tableNames.get(input.getFieldName()).getTableName());
                }
            }

            if(!query.getResultType().isAssignableFrom(Long.class) && sortRequest != null && (query.getOrderList() == null ||query.getOrderList().size() == 0)) {
                if (tableNames.get(sortRequest.getFieldName()).getTableName().equalsIgnoreCase(ShipmentDetails.class.getSimpleName())) {
                    if(sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                        query.orderBy(Arrays.asList(criteriaBuilder.desc(root.get(sortRequest.getFieldName()))));
                    } else {
                        query.orderBy(Arrays.asList(criteriaBuilder.asc(root.get(sortRequest.getFieldName()))));
                    }
                } else {
                    if(sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                        query.orderBy(Arrays.asList(criteriaBuilder.desc(((Join) root.fetch(tableNames.get(sortRequest.getFieldName()).getTableName(), JoinType.LEFT)).get(sortRequest.getFieldName()))));
                    } else {
                        query.orderBy(Arrays.asList(criteriaBuilder.asc(((Join) root.fetch(tableNames.get(sortRequest.getFieldName()).getTableName(), JoinType.LEFT)).get(sortRequest.getFieldName()))));
                    }
                }
            }
            return createSpecification(tableNames.get(input.getFieldName()).getDataType(), input, path, criteriaBuilder);

        };
    }

    public static <T> Predicate createSpecification(Class dataType, Criteria input, Path path, CriteriaBuilder criteriaBuilder) {
        switch (input.getOperator()){
            case "=":
                if(dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.equal(criteriaBuilder.lower(path.get(input.getFieldName())),(((String) input.getValue()).toLowerCase()));
                }
                return criteriaBuilder.equal(path.get(input.getFieldName()),input.getValue());

            case "!=":
                if(dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.notEqual(criteriaBuilder.lower(path.get(input.getFieldName())),(((String) input.getValue()).toLowerCase()));
                }
                return criteriaBuilder.notEqual(path.get(input.getFieldName()), input.getValue());

            case ">":
                if(dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.greaterThan(path.get(input.getFieldName()),(String) input.getValue());
                }
                if(dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.greaterThan(path.get(input.getFieldName()),covertStringToData((String) input.getValue(), "yyyy-MM-dd"));
                }
                if(dataType.isAssignableFrom(LocalDateTime.class)) {
                    return criteriaBuilder.greaterThan(path.get(input.getFieldName()),covertStringToLocalDate((String) input.getValue(), "yyyy-MM-dd"));
                }
                return criteriaBuilder.gt(path.get(input.getFieldName()),(Number) input.getValue());

            case "<":
                if(dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.lessThan(path.get(input.getFieldName()),(String) input.getValue());
                }
                if(dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.lessThan(path.get(input.getFieldName()),covertStringToData((String) input.getValue(), "yyyy-MM-dd"));
                }
                if(dataType.isAssignableFrom(LocalDateTime.class)) {
                    return criteriaBuilder.lessThan(path.get(input.getFieldName()),covertStringToLocalDate((String) input.getValue(), "yyyy-MM-dd"));
                }
                return criteriaBuilder.lt(path.get(input.getFieldName()), (Number) input.getValue());

            case "LIKE":
                return criteriaBuilder.like(criteriaBuilder.lower(path.get(input.getFieldName())),
                        "%"+((String) input.getValue()).toLowerCase()+"%");

            case "IN":
                return criteriaBuilder.in(path.get(input.getFieldName()))
                        .value(input.getValue());
            default:
                throw new RuntimeException("Operation not supported yet");
        }
    }

    public static <T> Specification<T> getSpecificationFromFilters(List<FilterCriteria> filter, SortRequest sortRequest, Map<String, Join<Class, ShipmentDetails>> map){
        if(filter == null || filter.size()==0) {
            return null;
        }

        Specification<T> specification = null;

        for (FilterCriteria input : filter) {
            if(input.getInnerFilter() != null && input.getInnerFilter().size() > 0) {
                if(input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(getSpecificationFromFilters(input.getInnerFilter(), null, map));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(getSpecificationFromFilters(input.getInnerFilter(), null, map));
                    }
                }
                else {
                    specification =
                            where(getSpecificationFromFilters(input.getInnerFilter(), sortRequest, map));
                }
            } else {
                if(input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(createSpecification(input.getCriteria(), null, map));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(createSpecification(input.getCriteria(), null, map));
                    }
                }else {
                    specification =
                            where(createSpecification(input.getCriteria(), sortRequest, map));
                }
            }
        }
        return specification;
    }

    public static LocalDateTime covertStringToLocalDate(String date, String pattern){

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);

        try {
            return LocalDate.parse(date, formatter).atStartOfDay();
        } catch (Exception e){
            return null;
        }
    }

    public static Date covertStringToData(String date, String pattern){

        SimpleDateFormat formatter = new SimpleDateFormat(pattern, Locale.ENGLISH);
        try {
            return formatter.parse(date);
        } catch (ParseException e) {
            return null;
        }
    }


//    protected TypedQuery<Long> getCountQuery(Specification<T> spec) {
//
//        CriteriaBuilder builder = em.getCriteriaBuilder();
//        CriteriaQuery<Long> query = builder.createQuery(Long.class);
//
//        Root<T> root = applySpecificationToCriteria(spec, query);
//
//        if (root.getFetches() != null && root.getFetches().size() != 0) {
//            for (Fetch fetch : root.getFetches()) {
//                root.getJoins().add((Join) fetch);
//            }
//            root.getFetches().clear();
//        }

}
