package com.dpw.runner.shipment.services.repository;

import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.dpw.runner.shipment.services.dto.request.SortRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.model.ShipmentTableMapping;
import com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancyRepository;
import com.dpw.runner.shipment.services.filter.PermissionsValidation.PermissionsRepository;
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
    Map<String, ShipmentTableMapping> tableNames = Map.ofEntries(
            Map.entry("type", ShipmentTableMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("orgId", ShipmentTableMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("houseBill", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("hblType", ShipmentTableMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("transportMode", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("releaseType", ShipmentTableMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", ShipmentTableMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("direction", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentType", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("status", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("source", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("jobType", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("serviceType", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("masterBill", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("bookingReference", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("consolRef", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("salesAgent", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(Long.class).build()),
            Map.entry("paymentTerms", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("incoterms", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentId", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("isDomestic", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).build()),
            Map.entry("assignedTo", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("additionalTerms", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("goodsDescription", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("createdAt", ShipmentTableMapping.builder().tableName("ShipmentDetails").dataType(Date.class).build()),
            Map.entry("estimatedPickup", ShipmentTableMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("actualPickup", ShipmentTableMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("estimatedDelivery", ShipmentTableMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("requiredBy", ShipmentTableMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("addressId", ShipmentTableMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("screeningStatus", ShipmentTableMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("paidPlace", ShipmentTableMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("placeOfIssue", ShipmentTableMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("dateOfIssue", ShipmentTableMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("dateOfReceipt", ShipmentTableMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("goodsCo", ShipmentTableMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("boeDate", ShipmentTableMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("boeNumber", ShipmentTableMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("shippingLine", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", ShipmentTableMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("weight", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("weightUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volume", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumeUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volumetricWeight", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumetricWeightUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("chargable", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("chargeableUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("netWeight", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("netWeightUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("noOfPacks", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("packsUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("innerPacks", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("innerPackUnit", ShipmentTableMapping.builder().tableName("measurementDetails").dataType(String.class).build())
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
