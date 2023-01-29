package App::BPOMUtils::NutritionFacts;

use 5.010001;
use strict 'subs', 'vars';
use utf8;
use warnings;
use Log::ger;

use Exporter 'import';

# AUTHORITY
# DATE
# DIST
# VERSION

our @EXPORT_OK = qw(
                       bpom_show_nutrition_facts
               );

our %SPEC;

$SPEC{':package'} = {
    v => 1.1,
    summary => 'Utilities related to BPOM nutrition facts',
};

my $res;

my $M = "\N{MULTIPLICATION SIGN}";

sub _nearest {
    require Math::Round;
    Math::Round::nearest(@_);
}

sub _fmt_num_id {
    require Number::Format;
    state $nf = Number::Format->new(THOUSANDS_SEP=>".", DECIMAL_POINT=>",");
    $nf->format_number(@_);
}

my @output_formats = (qw/
                                           raw_table
                                           vertical_html_table vertical_text_table
                                           linear_html linear_text raw_linear
                                           calculation_html calculation_text
/);
# horizontal_html_table horizontal_text_table formats not supported yet

$SPEC{bpom_show_nutrition_facts} = {
    v => 1.1,
    summary => 'Render BPOM-compliant nutrition fact table (ING - informasi nilai gizi) in various formats',
    args => {
        name => {schema=>'str*'},

        output_format => {
            summary => 'Pick an output format for the nutrition fact',
            schema => ['str*', {in=>\@output_formats}],
            description => <<'_',

`vertical_text_table` is the default. The /(vertical)?.*table/ formats presents
the information in a table, while the /linear/ formats presents the information
in a paragraph.

_
            default => 'vertical_text_table',
            cmdline_aliases => {
                f=>{},
            },
            tags => ['category:output'],
        },

        browser => {
            summary => 'View output HTML in browser instead of returning it',
            schema => 'true*',
            tags => ['category:output'],
        },

        color => {
            schema => ['str*', in=>[qw/always auto never/]],
            default => 'auto',
            tags => ['category:output'],
        },

        fat           => {summary => 'Total fat, in g/100g'           , schema => 'ufloat*', req=>1},
        saturated_fat => {summary => 'Saturated fat, in g/100g'       , schema => 'ufloat*', req=>1},
        protein       => {summary => 'Protein, in g/100g'             , schema => 'ufloat*', req=>1},
        carbohydrate  => {summary => 'Total carbohydrate, in g/100g'  , schema => 'ufloat*', req=>1},
        sugar         => {summary => 'Total sugar, in g/100g'         , schema => 'ufloat*', req=>1},
        sodium        => {summary => 'Sodium, in mg/100g'             , schema => 'ufloat*', req=>1, cmdline_aliases=>{salt=>{}}},

        serving_size  => {summary => 'Serving size, in g'             , schema => 'ufloat*', req=>1},
        package_size  => {summary => 'Packaging size, in g'           , schema => 'ufloat*', req=>1},
    },

    examples => [
        {
            summary => 'An example, in linear text format (color/emphasis is shown with markup)',
            args => {fat=>0.223, saturated_fat=>0.010, protein=>0.990, carbohydrate=>13.113, sugar=>7.173, sodium=>0.223, serving_size=>175, package_size=>20, output_format=>"linear_text", color=>"never"},
            test => 0,
        },
        {
            summary => 'An example, in raw_linear format (just like linear_text but with no border)',
            args => {fat=>0.223, saturated_fat=>0.010, protein=>0.990, carbohydrate=>13.113, sugar=>7.173, sodium=>0.223, serving_size=>175, package_size=>20, output_format=>"linear_text", color=>"never"},
            test => 0,
        },
        {
            summary => 'The same example in vertical HTML table format',
            args => {fat=>0.223, saturated_fat=>0.010, protein=>0.990, carbohydrate=>13.113, sugar=>7.173, sodium=>0.223, serving_size=>175, package_size=>20, output_format=>"vertical_html_table"},
            test => 0,
        },
        {
            summary => 'The same example, in vertical text format (color/emphasis is shown with markup)',
            args => {fat=>0.223, saturated_fat=>0.010, protein=>0.990, carbohydrate=>13.113, sugar=>7.173, sodium=>0.223, serving_size=>175, package_size=>20, output_format=>"vertical_text_table", color=>"never"},
            test => 0,
        },
        {
            summary => 'The same example, in calculation text format',
            args => {fat=>0.223, saturated_fat=>0.010, protein=>0.990, carbohydrate=>13.113, sugar=>7.173, sodium=>0.223, serving_size=>175, package_size=>20, output_format=>"calculation_text", color=>"never"},
            test => 0,
        },
    ],
};
sub bpom_show_nutrition_facts {
    my %args = @_;
    my $output_format = $args{output_format} // 'raw_table';
    return [400, "Unknown output format '$output_format'"] unless grep { $output_format eq $_ } @output_formats;

    my $color = $args{color} // 'auto';
    my $is_interactive = -t STDOUT; ## no critic: InputOutput::ProhibitInteractiveTest
    my $use_color = $color eq 'never' ? 0 : $color eq 'always' ? 1 : $is_interactive;

    my @rows;

    my $attr = $output_format =~ /html/ ? "raw_html" : "text";
    my $code_fmttext = sub {
        my $text = shift;
        if ($output_format =~ /html/) {
            require Org::To::HTML;
            my $res = Org::To::HTML::org_to_html(source_str => $text, naked=>1);
            die "Can't convert Org to HTML: $res->[0] - $res->[1]" if $res->[0] != 200;
            $res->[2];
        } else {
            my $res;
            if ($use_color) {
                require Org::To::ANSIText;
                $res = Org::To::ANSIText::org_to_ansi_text(source_str => $text);
                die "Can't convert Org to ANSI text: $res->[0] - $res->[1]" if $res->[0] != 200;
            } else {
                require Org::To::Text;
                $res = Org::To::Text::org_to_text(source_str => $text);
                die "Can't convert Org to text: $res->[0] - $res->[1]" if $res->[0] != 200;
            }
            $res->[2];
        }
    };

    my $per_package_ing = $args{serving_size} > $args{package_size} ? 1:0;
    my $size_key = $per_package_ing ? 'package_size' : 'serving_size';
    my $BR = $output_format =~ /html/ ? "<br />" : "\n";

    if ($output_format =~ /vertical/) {
        push @rows, [{colspan=>5, align=>'middle', $attr => $code_fmttext->("*INFORMASI NILAI GIZI*")}];
    } elsif ($output_format =~ /linear/) {
        if ($output_format =~ /html/) {
            push @rows, "<big><b>INFORMASI NILAI GIZI</b></big>&nbsp;&nbsp; ";
        } else {
            push @rows, $code_fmttext->("*INFORMASI NILAI GIZI*  ");
        }
    } elsif ($output_format =~ /calculation/) {
        push @rows, [{colspan=>2, align=>'middle', $attr => $code_fmttext->("*PERHITUNGAN INFORMASI NILAI GIZI*")}];
    }

    if ($per_package_ing) {
        if ($output_format =~ /vertical/) {
            push @rows, [{colspan=>5, text=>''}];
            push @rows, [{colspan=>5, align=>'left', $attr => $code_fmttext->("*JUMLAH PER KEMASAN ($args{package_size} g)*")}];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->(" *JUMLAH PER KEMASAN ($args{package_size} g)* : ");
        }
    } else {
        if ($output_format =~ /vertical/) {
            push @rows, [{colspan=>5, text=>''}];
            push @rows, [{colspan=>5, align=>'left', bottom_border=>1,
                          $attr =>
                          $code_fmttext->("Takaran saji "._fmt_num_id($args{serving_size})." g"). $BR .
                          $code_fmttext->(_fmt_num_id(_nearest(0.5, $args{package_size} / $args{serving_size}))." Sajian per Kemasan")
                      }];
            push @rows, [{colspan=>5, align=>'left', $attr => $code_fmttext->("*JUMLAH PER SAJIAN*")}];
            push @rows, [{colspan=>5, text=>''}];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->("Takaran saji : " . _fmt_num_id($args{serving_size}) . " g, " .
                                        _fmt_num_id(_nearest(0.5, $args{package_size} / $args{serving_size}))." Sajian per Kemasan *JUMLAH PER SAJIAN* : ");
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Sajian per kemasan*')}];
            push @rows, [{align=>'right', text=>'Sajian per kemasan'},
                         {align=>'left', $attr=>"= $args{package_size} / $args{serving_size} = ".($args{package_size}/$args{serving_size})}];
            push @rows, [{align=>'right', text=>"(dibulatkan 0,5 terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *"._nearest(0.5, $args{package_size} / $args{serving_size})."*")}];
        }
    }


  ENERGY: {
        my $code_round_energy = sub {
            my $val = shift;
            if ($val < 5)      { 0 }
            elsif ($val <= 50) { _nearest( 5, $val) }
            else               { _nearest(10, $val) }
        };

        my $val0 = $args{fat} * 9 + $args{protein} * 4 + $args{carbohydrate} * 4;
        my $val  = $val0*$args{$size_key}/100;
        my $valr = $code_round_energy->($val);
        my $pct_dv = $val/2150*100;
        my $pct_dv_R = _nearest(1, $pct_dv);
        if ($output_format eq 'raw_table') {
            push @rows, {
                name_eng => 'Total energy',
                name_ind => 'Energi total',
                val_per_100g  => $val0,
                (val_per_srv   => $val,
                 val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                (val_per_pkg   => $val,
                 val_per_pkg_R => $valr) x $per_package_ing,
                pct_dv        => $pct_dv,
                pct_dv_R      => $pct_dv_R,
            };
        } elsif ($output_format =~ /vertical/) {
            if ($per_package_ing) {
                push @rows, [{bottom_border=>1, colspan=>5, $attr=>$code_fmttext->("*Energi total $valr kkal*")}];
            } else {
                push @rows, [{colspan=>3, $attr=>$code_fmttext->("*Energi total*")}, {colspan=>2, align=>'right', $attr=>$code_fmttext->("*$valr kkal*")}];
            }
        } elsif ($output_format =~ /linear/) {
            if ($per_package_ing) {
                push @rows, $code_fmttext->("*Energi total $valr kkal*, ");
            } else {
                push @rows, $code_fmttext->("*Energi total $valr kkal*, ");
            }
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Energi total*')}];
            push @rows, [{align=>'right', text=>'Energi total per 100 g'},
                         {align=>'left', $attr=>"= lemak $M 9 + protein $M 4 + karbohidrat $M 4 = $args{fat} $M 9 + $args{protein} $M 4 + $args{carbohydrate} $M 4 = $val0 kkal"}];
            push @rows, [{align=>'right', text=>"Energi total per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                         {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val kkal"}];
            push @rows, [{align=>'right', text=>"(dibulatkan: <5 -> 0, <=50 -> 5 kkal terdekat, >50 -> 10 kkal terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$valr* kkal")}];
            push @rows, ['', ''];
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*%AKG energi total*')}];
            push @rows, [{align=>'right', text=>"\%AKG"},
                         {align=>'left', $attr=>"= $val / 2150 $M 100 = $pct_dv"}];
            push @rows, [{align=>'right', text=>"(dibulatkan ke % terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$pct_dv_R*")}];
        }

      ENERGY_FROM_FAT: {
            my $val0 = $args{fat} * 9;
            my $val  = $val0*$args{serving_size}/100;
            my $valr = $code_round_energy->($val);
            if ($output_format eq 'raw_table') {
                push @rows, {
                    name_eng => 'Energy from fat',
                    name_ind => 'Energi dari lemak',
                    val_per_100g  => $val0,
                    (val_per_srv   => $val,
                     val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                    (val_per_pkg   => $val,
                     val_per_pkg_R => $valr) x $per_package_ing,
                };
            } elsif ($output_format =~ /vertical/) {
                if ($per_package_ing) {
                } else {
                    push @rows, ['', {colspan=>2, $attr=>$code_fmttext->("Energi dari lemak")}, {colspan=>2, align=>'right', $attr=>$code_fmttext->("$valr kkal")}];
                }
            } elsif ($output_format =~ /linear/) {
                push @rows, $code_fmttext->("Energi dari lemak $valr kkal, ");
            } elsif ($output_format =~ /calculation/) {
                push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Energi dari lemak*')}];
                push @rows, [{align=>'right', text=>'Energi dari lemak per 100 g'},
                             {align=>'left', $attr=>"= lemak $M 9 = $args{fat} $M 9 = $val0 kkal"}];
                push @rows, [{align=>'right', text=>"Energi dari lemak per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                             {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val kkal"}];
                push @rows, [{align=>'right', text=>"(dibulatkan: <5 -> 0, <=50 -> 5 kkal terdekat, >50 -> 10 kkal terdekat)"},
                             {align=>'left', $attr=>$code_fmttext->("= *$valr* kkal")}];
            }
        }

      ENERGY_FROM_SATURATED_FAT: {
            my $val0 = $args{saturated_fat} * 9;
            my $val  = $val0*$args{$size_key}/100;
            my $valr = $code_round_energy->($val);
            if ($output_format eq 'raw_table') {
                push @rows, {
                    name_eng => 'Energy from saturated fat',
                    name_ind => 'Energi dari lemak jenuh',
                    val_per_100g  => $val0,
                    (val_per_srv   => $val,
                     val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                    (val_per_pkg   => $val,
                     val_per_pkg_R => $valr) x $per_package_ing,
                };
            } elsif ($output_format =~ /vertical/) {
                if ($per_package_ing) {
                } else {
                    push @rows, [{bottom_border=>1, text=>''}, {colspan=>2, $attr=>$code_fmttext->("Energi dari lemak jenuh")}, {colspan=>2, align=>'right', $attr=>$code_fmttext->("$valr kkal")}];
                }
            } elsif ($output_format =~ /linear/) {
                push @rows, $code_fmttext->("Energi dari lemak jenuh $valr kkal, ");
            } elsif ($output_format =~ /calculation/) {
                push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Energi dari lemak jenuh*')}];
                push @rows, [{align=>'right', text=>'Energi dari lemak per 100 g'},
                             {align=>'left', $attr=>"= lemak jenuh $M 9 = $args{saturated_fat} $M 9 = $val0 kkal"}];
                push @rows, [{align=>'right', text=>"Energi dari lemak jenuh per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                             {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val kkal"}];
                push @rows, [{align=>'right', text=>"(dibulatkan: <5 -> 0, <=50 -> 5 kkal terdekat, >50 -> 10 kkal terdekat)"},
                             {align=>'left', $attr=>$code_fmttext->("= *$valr* kkal")}];
            }
        }
    } # ENERGY

    if ($output_format eq 'raw_table') {
    } elsif ($output_format =~ /vertical/) {
        push @rows, [{colspan=>3, text=>''}, {colspan=>2, align=>'middle', $attr=>$code_fmttext->("*\% AKG**")}];
    } elsif ($output_format =~ /linear/) {
    }

  FAT: {
        my $code_round_fat = sub {
            my $val = shift;
            if ($val < 0.5)    { 0 }
            elsif ($val <= 5)  { sprintf("%.1f", _nearest(0.5, $val)) }
            else               { _nearest(1  , $val) }
        };
        my $code_round_fat_pct_dv = sub {
            my ($val, $valr) = @_;
            if ($valr == 0)    { 0 }
            else               { _nearest(1  , $val) }
        };

        my $val0 = $args{fat};
        my $val  = $val0*$args{$size_key}/100;
        my $valr = $code_round_fat->($val);
        my $pct_dv = $val/67*100;
        my $pct_dv_R = $code_round_fat_pct_dv->($pct_dv, $valr);
        if ($output_format eq 'raw_table') {
            push @rows, {
                name_eng => 'Total fat',
                name_ind => 'Lemak total',
                val_per_100g  => $val0,
                (val_per_srv   => $val,
                 val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                (val_per_pkg   => $val,
                 val_per_pkg_R => $valr) x $per_package_ing,
                pct_dv   => $pct_dv,
                pct_dv_R => $pct_dv_R,
            };
        } elsif ($output_format =~ /vertical/) {
            push @rows, [{colspan=>2, $attr=>$code_fmttext->("*Lemak total*")}, {align=>'right', $attr=>$code_fmttext->("*$valr g*")}, {align=>'right', $attr=>"$pct_dv_R %"}, ''];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->("*Lemak total $valr g ($pct_dv_R% AKG)*, ");
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Lemak total*')}];
            push @rows, [{align=>'right', text=>'Lemak total per 100 g'},
                         {align=>'left', $attr=>"= $args{fat} g"}];
            push @rows, [{align=>'right', text=>"Lemak total per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                         {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val g"}];
            push @rows, [{align=>'right', text=>"(dibulatkan: <0.5 -> 0, <=5 -> 0.5 g terdekat, >=5 -> 1 g terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$valr* g")}];
            push @rows, ['', ''];
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*%AKG lemak total*')}];
            push @rows, [{align=>'right', text=>"\%AKG"},
                         {align=>'left', $attr=>"= $val / 67 $M 100 = $pct_dv"}];
            push @rows, [{align=>'right', text=>"(dibulatkan ke % terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$pct_dv_R*")}];
        }

      SATURATED_FAT: {
            my $val0 = $args{saturated_fat};
            my $val  = $val0*$args{$size_key}/100;
            my $valr = $code_round_fat->($val);
            my $pct_dv = $val/20*100;
            my $pct_dv_R = $code_round_fat_pct_dv->($pct_dv, $valr);
            if ($output_format eq 'raw_table') {
                push @rows, {
                    name_eng => 'Saturated fat',
                    name_ind => 'Lemak jenuh',
                    val_per_100g  => $val0,
                    (val_per_srv   => $val,
                     val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                    (val_per_pkg   => $val,
                     val_per_pkg_R => $valr) x $per_package_ing,
                    pct_dv   => $pct_dv,
                    pct_dv_R => $pct_dv_R,
                };
            } elsif ($output_format =~ /vertical/) {
                push @rows, [{colspan=>2, $attr=>$code_fmttext->("*Lemak jenuh*")}, {align=>'right', $attr=>$code_fmttext->("*$valr g*")}, {align=>'right', $attr=>"$pct_dv_R %"}, ''];
            } elsif ($output_format =~ /linear/) {
                push @rows, $code_fmttext->("*Lemak jenuh $valr g ($pct_dv_R% AKG)*, ");
            } elsif ($output_format =~ /calculation/) {
                push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Lemak jenuh*')}];
                push @rows, [{align=>'right', text=>'Lemak jenuh per 100 g'},
                             {align=>'left', $attr=>"= $args{saturated_fat} g"}];
                push @rows, [{align=>'right', text=>"Lemak jenuh per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                             {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val g"}];
                push @rows, [{align=>'right', text=>"(dibulatkan: <0.5 -> 0, <=5 -> 0.5 g terdekat, >=5 -> 1 g terdekat)"},
                             {align=>'left', $attr=>$code_fmttext->("= *$valr* g")}];
                push @rows, ['', ''];
                push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*%AKG lemak jenuh*')}];
                push @rows, [{align=>'right', text=>"\%AKG"},
                             {align=>'left', $attr=>"= $val / 67 $M 100 = $pct_dv"}];
                push @rows, [{align=>'right', text=>"(dibulatkan ke % terdekat)"},
                             {align=>'left', $attr=>$code_fmttext->("= *$pct_dv_R*")}];
            }
    } # FAT

  PROTEIN: {
        my $code_round_protein = sub {
            my $val = shift;
            if ($val < 0.5)    { 0 }
            else               { _nearest(1  , $val) }
        };
        my $code_round_protein_pct_dv = sub {
            my ($val, $valr) = @_;
            if   ($valr == 0)  { 0 }
            else               { _nearest(1  , $val) }
        };

        my $val0 = $args{protein};
        my $val  = $val0*$args{$size_key}/100;
        my $valr = $code_round_protein->($val);
        my $pct_dv = $val/60*100;
        my $pct_dv_R = $code_round_protein_pct_dv->($pct_dv, $valr);
        if ($output_format eq 'raw_table') {
            push @rows, {
                name_eng => 'Protein',
                name_ind => 'Protein',
                val_per_100g  => $val0,
                (val_per_srv   => $val,
                 val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                (val_per_pkg   => $val,
                 val_per_pkg_R => $valr) x $per_package_ing,
                pct_dv   => $pct_dv,
                pct_dv_R => $pct_dv_R,
            };
        } elsif ($output_format =~ /vertical/) {
            push @rows, [{colspan=>2, $attr=>$code_fmttext->("*Protein*")}, {align=>'right', $attr=>$code_fmttext->("*$valr g*")}, {align=>'right', $attr=>"$pct_dv_R %"}, ''];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->("*Protein $valr g ($pct_dv_R% AKG)*, ");
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Protein*')}];
            push @rows, [{align=>'right', text=>'Protein per 100 g'},
                         {align=>'left', $attr=>"= $args{protein} g"}];
            push @rows, [{align=>'right', text=>"Protein total per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                         {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val g"}];
            push @rows, [{align=>'right', text=>"(dibulatkan: <0.5 -> 0, >=0.5 -> 1 g terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$valr* g")}];
            push @rows, ['', ''];
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*%AKG protein*')}];
            push @rows, [{align=>'right', text=>"\%AKG"},
                         {align=>'left', $attr=>"= $val / 60 $M 100 = $pct_dv"}];
            push @rows, [{align=>'right', text=>"(dibulatkan ke % terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$pct_dv_R*")}];
        }
    }

  CARBOHYDRATE: {
        my $code_round_carbohydrate = sub {
            my $val = shift;
            if ($val < 0.5)    { 0 }
            else               { _nearest(1  , $val) }
        };
        my $code_round_carbohydrate_pct_dv = sub {
            my ($val, $valr) = @_;
            if ($valr == 0)    { 0 }
            else               { _nearest(1  , $val) }
        };

        my $val0 = $args{carbohydrate};
        my $val  = $val0*$args{$size_key}/100;
        my $valr = $code_round_carbohydrate->($val);
        my $pct_dv_R = $code_round_carbohydrate_pct_dv->($val/325*100, $valr);
        if ($output_format eq 'raw_table') {
            push @rows, {
                name_eng => 'Total carbohydrate',
                name_ind => 'Karbohidrat total',
                val_per_100g  => $val0,
                (val_per_srv   => $val,
                 val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                (val_per_pkg   => $val,
                 val_per_pkg_R => $valr) x $per_package_ing,
                pct_dv   => $val/325*100,
                pct_dv_R => $pct_dv_R,
            };
        } elsif ($output_format =~ /vertical/) {
            push @rows, [{colspan=>2, $attr=>$code_fmttext->("*Karbohidrat total*")}, {align=>'right', $attr=>$code_fmttext->("*$valr g*")}, {align=>'right', $attr=>"$pct_dv_R %"}, ''];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->("*Karbohidrat total $valr g ($pct_dv_R% AKG)*, ");
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Karbohidrat total*')}];
            push @rows, [{align=>'right', text=>'Karbohidrat total per 100 g'},
                         {align=>'left', $attr=>"= $args{carbohydrate} g"}];
            push @rows, [{align=>'right', text=>"Karbohidrat total per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                         {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val g"}];
            push @rows, [{align=>'right', text=>"(dibulatkan: <0.5 -> 0, >=0.5 -> 1 g terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$valr* g")}];
            push @rows, ['', ''];
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*%AKG karbohidrat total*')}];
            push @rows, [{align=>'right', text=>"\%AKG"},
                         {align=>'left', $attr=>"= $val / 325 $M 100 = $pct_dv"}];
            push @rows, [{align=>'right', text=>"(dibulatkan ke % terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$pct_dv_R*")}];
        }
    }

  SUGAR: {
        my $code_round_sugar = sub {
            my $val = shift;
            if ($val < 0.5)    { 0 }
            else               { _nearest(1  , $val) }
        };

        my $val0 = $args{sugar};
        my $val  = $val0*$args{$size_key}/100;
        my $valr = $code_round_sugar->($val);
        if ($output_format eq 'raw_table') {
            push @rows, {
                name_eng => 'Total sugar',
                name_ind => 'Gula total',
                val_per_100g  => $val0,
                (val_per_srv   => $val,
                 val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                (val_per_pkg   => $val,
                 val_per_pkg_R => $valr) x $per_package_ing,
            };
        } elsif ($output_format =~ /vertical/) {
            push @rows, [{colspan=>2, $attr=>$code_fmttext->("*Gula*")}, {align=>'right', $attr=>$code_fmttext->("*$valr g*")}, '', ''];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->("*Gula $valr g*, ");
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Gula*')}];
            push @rows, [{align=>'right', text=>'Gula per 100 g'},
                         {align=>'left', $attr=>"= $args{sugar} g"}];
            push @rows, [{align=>'right', text=>"Gula per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                         {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val g"}];
            push @rows, [{align=>'right', text=>"(dibulatkan: <0.5 -> 0, >=0.5 -> 1 g terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$valr* g")}];
        }
    }

  SODIUM: {
        my $code_round_sodium = sub {
            my $val = shift;
            if ($val < 5)       { 0 }
            elsif ($val <= 140) { _nearest( 5, $val) }
            else                { _nearest(10, $val) }
        };
        my $code_round_sodium_pct_dv = sub {
            my ($val, $fat_valr) = @_;
            if ($fat_valr == 0) { 0 }
            else                { _nearest(1  , $val) }
        };

        my $val0 = $args{sodium};
        my $val  = $val0*$args{$size_key}/100;
        my $valr = $code_round_sodium->($val);
        my $pct_dv = $val/1500*100;
        my $pct_dv_R = $code_round_sodium_pct_dv->($val/1500*100, $valr);
        if ($output_format eq 'raw_table') {
            push @rows, {
                name_eng => 'Salt (Sodium)',
                name_ind => 'Garam (Natrium)',
                val_per_100g  => $val0,
                (val_per_srv   => $val,
                 val_per_srv_R => $valr) x ($per_package_ing ? 0:1),
                (val_per_pkg   => $val,
                 val_per_pkg_R => $valr) x $per_package_ing,
                pct_dv   => $val/325*100,
                pct_dv_R => $pct_dv_R,
            };
        } elsif ($output_format =~ /vertical/) {
            push @rows, [{bottom_border=>1, colspan=>2, $attr=>$code_fmttext->("*Garam (Natrium)*")}, {align=>'right', $attr=>$code_fmttext->("*$valr mg*")}, {align=>'right', $attr=>"$pct_dv_R %"}, ''];
        } elsif ($output_format =~ /linear/) {
            push @rows, $code_fmttext->("*Garam (Natrium) $valr mg ($pct_dv_R% AKG)*. ");
        } elsif ($output_format =~ /calculation/) {
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*Natrium*')}];
            push @rows, [{align=>'right', text=>'Natrium per 100 g'},
                         {align=>'left', $attr=>"= $args{sodium} mg"}];
            push @rows, [{align=>'right', text=>"Natrium per ".($per_package_ing ? "kemasan $args{package_size} g" : "takaran saji $args{serving_size} g")},
                         {align=>'left', $attr=>"= $val0 $M $args{$size_key} / 100 = $val mg"}];
            push @rows, [{align=>'right', text=>"(dibulatkan: <5 -> 0, <=140 -> 5 mg terdekat, >140 -> 10 mg terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$valr* mg")}];
            push @rows, ['', ''];
            push @rows, [{colspan=>2, align=>'middle', $attr=>$code_fmttext->('*%AKG natrium*')}];
            push @rows, [{align=>'right', text=>"\%AKG"},
                         {align=>'left', $attr=>"= $val / 1500 $M 100 = $pct_dv"}];
            push @rows, [{align=>'right', text=>"(dibulatkan ke % terdekat)"},
                         {align=>'left', $attr=>$code_fmttext->("= *$pct_dv_R*")}];
        }
        }
    }

    if ($output_format eq 'raw_table') {
    } elsif ($output_format =~ /vertical/) {
        push @rows, [{colspan=>5, $attr=>$code_fmttext->("/*Persen AKG berdasarkan kebutuhan energi 2150 kkal. Kebutuhan energi Anda mungkin lebih tinggi atau lebih rendah./")}];
    } elsif ($output_format =~ /linear/) {
        push @rows, $code_fmttext->(                      "/Persen AKG berdasarkan kebutuhan energi 2150 kkal. Kebutuhan energi Anda mungkin lebih tinggi atau lebih rendah./");
    }


  OUTPUT:
    if ($output_format eq 'raw_table') {
        return [200, "OK", \@rows, {'table.fields'=>[qw/name_eng name_ind val_per_100g val_per_srv val_per_srv_R val_per_pkg val_per_pkg_R pct_dv pct_dv_R/]}];
    }

    my $text;
    if ($output_format =~ /vertical/) {
        if ($output_format =~ /html/) {
            require Text::Table::HTML;
            my $table = Text::Table::HTML::table(rows => \@rows, header_row=>0);
            $table =~ s!<table>!<table class="$output_format"><colgroup><col style="width:16pt;"><col style="width:200pt;"><col style="width:48pt;"><col style="width:48pt;"><col style="width:36pt;"></colgroup>!;
            $text = "
<style>
  table.$output_format { border-collapse: collapse; border: solid 1pt black; }
  table.$output_format tr.has_bottom_border { border-bottom: solid 1pt black; }
</style>\n" . $table;
        } else {
            require Text::Table::More;
            $text = Text::Table::More::generate_table(rows => \@rows, color=>1, header_row=>0);
        }
    } elsif ($output_format =~ /linear/) {
        if ($output_format =~ /html/) {
            $text = "
<style>
  p.$output_format { border: solid 1pt black; }
</style>
<p class=\"$output_format\">" . join("", @rows). "</p>\n";
        } elsif ($output_format =~ /text/) {
            require Text::ANSI::Util;
            require Text::Table::More;
            my $ing = Text::ANSI::Util::ta_wrap(join("", @rows), $ENV{COLUMNS} // 80);
            $text = Text::Table::More::generate_table(rows => [[$ing]], header_row=>0);
        } else {
            # raw_linear
            $text = join("", @rows) . "\n";
        }
    } elsif ($output_format =~ /calculation/) {
        if ($output_format =~ /html/) {
            require Text::Table::HTML;
            my $table = Text::Table::HTML::table(rows => \@rows, header_row=>0);
            $table =~ s!<table>!<table class="$output_format">!;
            $text = "
<style>
  table.$output_format { font-size: smaller; border-collapse: collapse; border: solid 1pt black; }
  table.$output_format tr.has_bottom_border { border-bottom: solid 1pt black; }
</style>\n" . $table;
        } else {
            require Text::Table::More;
            $text = Text::Table::More::generate_table(rows => \@rows, color=>1, header_row=>0);
        }
    }

    if ($output_format =~ /html/ && $args{browser}) {
        require Browser::Open;
        require File::Slurper;
        require File::Temp;

        my $tempdir = File::Temp::tempdir();
        my $temppath = "$tempdir/ing.html";
        File::Slurper::write_text($temppath, $text);

        my $url = "file:$temppath";
        my $err = Browser::Open::open_browser($url);
        return [500, "Can't open browser"] if $err;
        return [200];
    }

    return [200, "OK", $text, {'cmdline.skip_format'=>1}];
}

1;
#ABSTRACT:

=head1 SYNOPSIS


=head1 DESCRIPTION

This distribution includes CLI utilities related to BPOM nutrition facts.

# INSERT_EXECS_LIST


=head1 SEE ALSO

L<https://pom.go.id>

L<App::BPOMUtils>

=cut
