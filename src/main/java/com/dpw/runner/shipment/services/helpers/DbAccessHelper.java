package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.utils.ObjectUtility;
import com.nimbusds.jose.util.Pair;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.springframework.data.jpa.domain.Specification.where;

@SuppressWarnings("ALL")
public class DbAccessHelper {
    private static Map<String, RunnerEntityMapping> tableNames = new HashMap<>();

    public static <T> Pair<Specification<T>, Pageable> fetchData(ListCommonRequest request, Class className, Map<String, RunnerEntityMapping> tableName) {
        tableNames = tableName;
        Pageable pages;
        if (request.getSortRequest() != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() == 0) {
            Sort sortRequest = Sort.by(tableNames.get(request.getSortRequest().getFieldName()) + "." + request.getSortRequest().getFieldName());
            sortRequest = sortRequest.descending();
            pages = PageRequest.of(request.getPageNo(), request.getLimit(), sortRequest);
        } else {
            pages = PageRequest.of(request.getPageNo(), request.getLimit());
        }
        List<FilterCriteria> filterCriteria = (request.getFilterCriteria() == null ? new ArrayList<FilterCriteria>() : request.getFilterCriteria());
        SortRequest sortRequest = request.getSortRequest();

        Specification<T> specification = null;
        Map<String, Join<Class, T>> map = new HashMap<>();
        for (FilterCriteria filters : filterCriteria) {
            if (filters.getLogicOperator() == null) {
                specification =
                        where(getSpecificationFromFilters(filters.getInnerFilter(), sortRequest, map, className.getSimpleName()));
            } else if (filters.getLogicOperator().equalsIgnoreCase("OR")) {
                specification = specification.or(getSpecificationFromFilters(filters.getInnerFilter(), null, map, className.getSimpleName()));
            } else if (filters.getLogicOperator().equalsIgnoreCase("AND")) {
                specification = specification.and(getSpecificationFromFilters(filters.getInnerFilter(), null, map, className.getSimpleName()));
            }
        }
        return Pair.of(specification, pages);
    }

    public static <T> Pair<Specification<T>, Pageable> fetchData(ListCommonRequest request, Class className) {
        Pageable pages;
        if (request.getSortRequest() != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() == 0) {
            Sort sortRequest = Sort.by(request.getSortRequest().getFieldName());
            sortRequest = sortRequest.descending();
            pages = PageRequest.of(request.getPageNo(), request.getLimit(), sortRequest);
        } else {
            pages = PageRequest.of(request.getPageNo(), request.getLimit());
        }
        List<FilterCriteria> filterCriteria = (request.getFilterCriteria() == null ? new ArrayList<FilterCriteria>() : request.getFilterCriteria());
        SortRequest sortRequest = request.getSortRequest();

        Map<String, Class> dataTypeMap = new HashMap<>();
        ObjectUtility.getAllFields(className, dataTypeMap);

        Specification<T> specification = null;
        Map<String, Join<Class, T>> map = new HashMap<>();
        for (FilterCriteria filters : filterCriteria) {
            if (filters.getLogicOperator() == null) {
                specification =
                        where(getSpecificationFromFiltersWithoutMapping(filters.getInnerFilter(), sortRequest, map, className.getSimpleName(), dataTypeMap));
            } else if (filters.getLogicOperator().equalsIgnoreCase("OR")) {
                specification = specification.or(getSpecificationFromFiltersWithoutMapping(filters.getInnerFilter(), null, map, className.getSimpleName(), dataTypeMap));
            } else if (filters.getLogicOperator().equalsIgnoreCase("AND")) {
                specification = specification.and(getSpecificationFromFiltersWithoutMapping(filters.getInnerFilter(), null, map, className.getSimpleName(), dataTypeMap));
            }
        }
        return Pair.of(specification, pages);
    }

    private static <T> Specification<T> getSpecificationFromFilters(List<FilterCriteria> filter, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className) {
        if (filter == null || filter.size() == 0) {
            return null;
        }

        Specification<T> specification = null;

        for (FilterCriteria input : filter) {
            if (input.getInnerFilter() != null && input.getInnerFilter().size() > 0) {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(getSpecificationFromFilters(input.getInnerFilter(), null, map, className));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(getSpecificationFromFilters(input.getInnerFilter(), null, map, className));
                    }
                } else {
                    specification =
                            where(getSpecificationFromFilters(input.getInnerFilter(), sortRequest, map, className));
                }
            } else {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(createSpecification(input.getCriteria(), null, map, className));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(createSpecification(input.getCriteria(), null, map, className));
                    }
                } else {
                    specification =
                            where(createSpecification(input.getCriteria(), sortRequest, map, className));
                }
            }
        }
        return specification;
    }

    private static <T> Specification<T> createSpecification(Criteria input, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className) {
        return (root, query, criteriaBuilder) -> {
            Path path = null;

            if (tableNames.get(input.getFieldName()).getTableName().equalsIgnoreCase(className)) {
                path = root;
            } else {
                if (root.getJoins() == null || root.getJoins().size() == 0 || map.get(tableNames.get(input.getFieldName()).getTableName()) == null ||
                        !root.getJoins().contains(map.get(tableNames.get(input.getFieldName()).getTableName()))) {
                    Join<Class, T> join = root.join(tableNames.get(input.getFieldName()).getTableName(), JoinType.LEFT);
                    map.put(tableNames.get(input.getFieldName()).getTableName(), join);
                    path = join;
                    query.distinct(true);
                } else {
                    path = map.get(tableNames.get(input.getFieldName()).getTableName());
                }
            }

            if (!query.getResultType().isAssignableFrom(Long.class) && sortRequest != null && (query.getOrderList() == null || query.getOrderList().size() == 0)) {
                if (tableNames.get(sortRequest.getFieldName()).getTableName().equalsIgnoreCase(className)) {
                    if (sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                        query.orderBy(Arrays.asList(criteriaBuilder.desc(root.get(sortRequest.getFieldName()))));
                    } else {
                        query.orderBy(Arrays.asList(criteriaBuilder.asc(root.get(sortRequest.getFieldName()))));
                    }
                } else {
                    if (sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                        query.orderBy(Arrays.asList(criteriaBuilder.desc(((Join) root.fetch(tableNames.get(sortRequest.getFieldName()).getTableName(), JoinType.LEFT)).get(sortRequest.getFieldName()))));
                    } else {
                        query.orderBy(Arrays.asList(criteriaBuilder.asc(((Join) root.fetch(tableNames.get(sortRequest.getFieldName()).getTableName(), JoinType.LEFT)).get(sortRequest.getFieldName()))));
                    }
                }
            }
            return createSpecification(tableNames.get(input.getFieldName()).getDataType(), input, path, criteriaBuilder);

        };
    }

    private static <T> Predicate createSpecification(Class dataType, Criteria input, Path path, CriteriaBuilder criteriaBuilder) {
        switch (input.getOperator()) {
            case "=":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.equal(criteriaBuilder.lower(path.get(input.getFieldName())), (((String) input.getValue()).toLowerCase()));
                }
                return criteriaBuilder.equal(path.get(input.getFieldName()), input.getValue());

            case "!=":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.notEqual(criteriaBuilder.lower(path.get(input.getFieldName())), (((String) input.getValue()).toLowerCase()));
                }
                return criteriaBuilder.notEqual(path.get(input.getFieldName()), input.getValue());

            case ">":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.greaterThan(path.get(input.getFieldName()), (String) input.getValue());
                }
                if (dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.greaterThan(path.get(input.getFieldName()), covertStringToData((String) input.getValue(), "yyyy-MM-dd"));
                }
                if (dataType.isAssignableFrom(LocalDateTime.class)) {
                    return criteriaBuilder.greaterThan(path.get(input.getFieldName()), covertStringToLocalDate((String) input.getValue(), "yyyy-MM-dd"));
                }
                return criteriaBuilder.gt(path.get(input.getFieldName()), (Number) input.getValue());

            case "<":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.lessThan(path.get(input.getFieldName()), (String) input.getValue());
                }
                if (dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.lessThan(path.get(input.getFieldName()), covertStringToData((String) input.getValue(), "yyyy-MM-dd"));
                }
                if (dataType.isAssignableFrom(LocalDateTime.class)) {
                    return criteriaBuilder.lessThan(path.get(input.getFieldName()), covertStringToLocalDate((String) input.getValue(), "yyyy-MM-dd"));
                }
                return criteriaBuilder.lt(path.get(input.getFieldName()), (Number) input.getValue());

            case "LIKE":
                return criteriaBuilder.like(criteriaBuilder.lower(path.get(input.getFieldName())),
                        "%" + ((String) input.getValue()).toLowerCase() + "%");

            case "IN":
                return criteriaBuilder.in(path.get(input.getFieldName()))
                        .value(input.getValue());
            default:
                throw new RuntimeException("Operation not supported yet");
        }
    }

    private static <T> Specification<T> getSpecificationFromFiltersWithoutMapping(List<FilterCriteria> filter, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className, Map<String, Class> dataTypeMap) {
        if (filter == null || filter.size() == 0) {
            return null;
        }

        Specification<T> specification = null;

        for (FilterCriteria input : filter) {
            if (input.getInnerFilter() != null && input.getInnerFilter().size() > 0) {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(getSpecificationFromFiltersWithoutMapping(input.getInnerFilter(), null, map, className, dataTypeMap));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(getSpecificationFromFiltersWithoutMapping(input.getInnerFilter(), null, map, className, dataTypeMap));
                    }
                } else {
                    specification =
                            where(getSpecificationFromFiltersWithoutMapping(input.getInnerFilter(), sortRequest, map, className, dataTypeMap));
                }
            } else {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(createSpecificationWithoutFilter(input.getCriteria(), null, map, className, dataTypeMap));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(createSpecificationWithoutFilter(input.getCriteria(), null, map, className, dataTypeMap));
                    }
                } else {
                    specification =
                            where(createSpecificationWithoutFilter(input.getCriteria(), sortRequest, map, className, dataTypeMap));
                }
            }
        }
        return specification;
    }

    private static <T> Specification<T> createSpecificationWithoutFilter(Criteria input, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className, Map<String, Class> dataTypeMap) {
        return (root, query, criteriaBuilder) -> {
            Path path = root;

            if (!query.getResultType().isAssignableFrom(Long.class) && sortRequest != null && (query.getOrderList() == null || query.getOrderList().size() == 0)) {
                if (sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                    query.orderBy(Arrays.asList(criteriaBuilder.desc(root.get(sortRequest.getFieldName()))));
                } else {
                    query.orderBy(Arrays.asList(criteriaBuilder.asc(root.get(sortRequest.getFieldName()))));
                }
            }
            return createSpecification(dataTypeMap.get(input.getFieldName()), input, path, criteriaBuilder);

        };
    }

    private static LocalDateTime covertStringToLocalDate(String date, String pattern) {

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);

        try {
            return LocalDate.parse(date, formatter).atStartOfDay();
        } catch (Exception e) {
            return null;
        }
    }

    private static Date covertStringToData(String date, String pattern) {

        SimpleDateFormat formatter = new SimpleDateFormat(pattern, Locale.ENGLISH);
        try {
            return formatter.parse(date);
        } catch (ParseException e) {
            return null;
        }
    }
}
