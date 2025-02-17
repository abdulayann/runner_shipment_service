package com.dpw.runner.shipment.services.commons.constants;

public class DBQueryConstants {
    public static final String ELDETAILS_SELECT_ELNUMBERS_QUERY = "SELECT e FROM ELDetails e WHERE e.elNumber = :elNumber";

    private DBQueryConstants() {
    }
}
